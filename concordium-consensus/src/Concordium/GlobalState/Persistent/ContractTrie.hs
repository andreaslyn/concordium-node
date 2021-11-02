{-# LANGUAGE ScopedTypeVariables #-}

module Concordium.GlobalState.Persistent.ContractTrie (Trie, Key, empty, isEmpty, singleton, tlookup, alter, tinsert, tdelete, tupdate) where

import Data.Word ( Word8 )
import Data.Maybe (isNothing)
import Data.Bifunctor (first, bimap)
import qualified Data.List as L
import Concordium.GlobalState.Persistent.BlobStore (BufferedRef, makeBufferedRef, BlobStorable (storeUpdate, store, load), loadBufferedRef)
import Control.Monad.IO.Class ( MonadIO )
import Data.Serialize (getWord8, Get, getWord32be, Put, putWord8, putWord32be)
import Control.Monad (replicateM)

type Prefix = [Word8]
type Key = [Word8]
type Branch a = (Prefix, BufferedRef (Trie a))

data Trie a = Trie { value :: Maybe a
                   , branches :: [Branch a] }
              deriving (Show)

instance (BlobStorable m a) => BlobStorable m (Trie a) where
  store node = fst <$> storeUpdate node
  storeUpdate node = do
    -- Store the value if any
    (putValue, v) <- case value node of
      Just v -> bimap (putWord8 1 <>) Just <$> storeUpdate v
      Nothing -> return (putWord8 0, Nothing)
    -- Store the branches
    let bs = branches node
    let branchLength = putWord8 $ fromIntegral $ L.length bs
    storeBs <- mapM storeUpdateBranch bs
    -- Construct the storeUpdatedNode
    let putNode = putValue >> branchLength >> sequence_ (fst <$> storeBs)
    let updatedNode = Trie {value = v, branches = snd <$> storeBs}
    return (putNode, updatedNode)

    where
      -- | storeUpdate for a single branch of the node
      storeUpdateBranch :: Branch a -> m (Put, Branch a)
      storeUpdateBranch (prefix, ref) = do
        (refPut, updatedRef) <- storeUpdate ref
        let putPrefixLength = putWord32be $ fromIntegral $ L.length prefix
        let putBranch = putPrefixLength >> mapM_ putWord8 prefix >> refPut
        let updatedBranch = (prefix, updatedRef)
        return (putBranch, updatedBranch)

  load = do
    -- Load the nodes value if any
    v <- loadValue
    -- Load branches
    bs <- loadBranches
    -- Construct the node
    return $ do v' <- v
                bs' <- bs
                return Trie {value = v', branches = bs'}

    where
      loadValue :: Get (m (Maybe a))
      loadValue = do
         valueTag <- getWord8
         case valueTag of
           0 -> return $ return Nothing
           1 -> (fmap . fmap) Just load
           _ -> fail "Invalid trie node value serialization"

      loadBranch :: Get (m (Branch a))
      loadBranch = do
        prefixLength <- fromIntegral <$> getWord32be
        prefix <- replicateM prefixLength getWord8
        ref :: m (BufferedRef (Trie a)) <- load
        return $ (prefix, ) <$> ref

      loadBranches :: Get (m [Branch a])
      loadBranches = do
        branchLength <- fromIntegral <$> getWord8
        sequence <$> replicateM branchLength loadBranch

empty :: Trie a
empty = Trie { value = Nothing, branches = [] }

isEmpty :: Trie a -> Bool
isEmpty trie = case trie of
  Trie { value = Nothing, branches = [] } -> True
  _ -> False

gotSingleBranch :: Trie a -> Bool
gotSingleBranch trie = case branches trie of
  [_] -> True
  _ -> False

makeValueNode :: a -> Trie a
makeValueNode value = Trie {value = Just value, branches = []}

makeBranchNode :: [Branch a] -> Trie a
makeBranchNode branches = Trie{value = Nothing, branches = branches}

makeBranch :: MonadIO m => Prefix -> Trie a -> m (Branch a)
makeBranch prefix node = do bnode <- makeBufferedRef node
                            return (prefix, bnode)

singleton :: MonadIO m => Prefix -> a -> m (Trie a)
singleton prefix value =
  case prefix of
    [] -> return $ makeValueNode value
    _ -> do branch <- makeBranch prefix $ makeValueNode value
            return $ makeBranchNode [ branch ]

tlookup :: BlobStorable m a => Key -> Trie a -> m (Maybe a)
tlookup key node =
    case key of
      [] -> return $ value node
      (k : _) ->
        case branchWithPrefix of
             Just (remainingKey, nodeRef) -> do branchNode <- loadBufferedRef nodeRef
                                                tlookup remainingKey branchNode
             Nothing -> return Nothing
        where
          branchWithPrefix =
            do (branchPrefix, branch) <- L.find ((== k) . head . fst) (branches node)
               let (_, remainingKey, remainingBranchPrefix) = key `sharedPrefixOf` branchPrefix
               if L.null remainingBranchPrefix then
                 Just (remainingKey, branch)
               else
                 Nothing

sharedPrefixOf :: Prefix -> Prefix -> (Prefix, Prefix, Prefix)
sharedPrefixOf keyL keyR =
    case (keyL, keyR) of
      (x:xs, y:ys) | x == y -> let (p, xs', ys') = sharedPrefixOf xs ys in (x:p, xs', ys')
      (xs, ys) -> ([], xs, ys)

-- | A generalized update function which can be used to insert, delete and update values of the map given some key.
-- The update function will receive the current value (Just a) or Nothing if the value is currently not in the map.
-- If the result of the function will replace the value, if the result is Nothing the key will be removed.
--
-- It will ensure to compress branches when inserting and removing keys.
--
-- Assumes each node have branches sorted by their prefix, likewise it ensures inserted branches follow this.
alter :: BlobStorable m a => (Maybe a -> Maybe a) -> Key -> Trie a -> m (Trie a)
alter alterFn key node =
    case key of
      -- No key left means we have found the current node for the key
      [] -> return node { value = alterFn (value node) }
      -- Using the first part of the key we search and split the node branches right before the key which matches.
      (k : _) -> let (less, greaterOrEq) = L.span ((< k) . head . fst) (branches node) in
        case greaterOrEq of
          -- No branch with prefix equal to this or higher, so the key is not in the map.
          [] ->
            whenInsertValue (\value -> do branch <- makeBranch key $ makeValueNode value
                                          return $ node {branches = less ++ [branch] })
          -- Some branches with prefix eq or higher
          ((branchPrefix, nodeRef):greater) ->
             let (shared, remainingKey, remainingBranchPrefix) = key `sharedPrefixOf` branchPrefix in

               -- If they share no prefix: insert a new branch with this prefix.
               if L.null shared then
                 whenInsertValue (\value ->
                     do newBranch <- makeBranch remainingKey $ makeValueNode value
                        return node {branches = less ++ newBranch:greaterOrEq})

               -- If the key takes up all of the branch prefix: recurse in the branch with the remaining key and check for possible path compression (Should only be relevant when deleting keys).
               else if L.null remainingBranchPrefix then
                 do branchNode <- loadBufferedRef nodeRef
                    updatedBranchNode <- alter alterFn remainingKey branchNode
                    -- If the updated branch is now empty we remove it.
                    if isEmpty updatedBranchNode then
                      return node {branches = less ++ greater}
                    -- The updated branch have only a single branch and no value, so we compress.
                    else if gotSingleBranch updatedBranchNode && isNothing (value updatedBranchNode) then
                       let updatedSubBranch = head $ branches updatedBranchNode
                           updatedBranch = first (branchPrefix ++) updatedSubBranch
                       in return node {branches = less ++ updatedBranch:greater}
                    -- Otherwise we simply replace the branch with the updated one.
                    else
                       do updatedBranch <- makeBranch branchPrefix updatedBranchNode
                          return node {branches = less ++ updatedBranch:greater}

               -- They share some prefix, but not all of the branch prefix: insert a new node splitting the prefix and then add the branch and the new value as branches.
               else
                 whenInsertValue (\value ->
                     do let updatedBranch = (remainingBranchPrefix, nodeRef)
                        newBranch <- makeBranch remainingKey $ makeValueNode value
                        let splitBranches = if head remainingKey < head remainingBranchPrefix then [newBranch, updatedBranch] else [updatedBranch, newBranch]
                        splitBranch <- makeBranch shared $ makeBranchNode splitBranches
                        return node {branches = less ++ splitBranch:greater})
  where
    -- | Run the alterFn with Nothing and return the node unmodified if the result is Nothing otherwise it calls the provided function with the result.
    -- Assumes the map did not contain
    whenInsertValue insertFn = case alterFn Nothing of
       Nothing -> return node
       Just value -> insertFn value

tinsert ::  BlobStorable m a => Key -> a -> Trie a -> m (Trie a)
tinsert key value = alter (const (Just value)) key

tdelete :: BlobStorable m a => Key -> Trie a -> m (Trie a)
tdelete = alter (const Nothing)

tupdate :: BlobStorable m a => (a -> Maybe a) -> Key -> Trie a -> m (Trie a)
tupdate update = alter (>>= update)
