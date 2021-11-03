{-# LANGUAGE ScopedTypeVariables #-}

module Concordium.GlobalState.Persistent.ContractTrie (Trie, Key, empty, isEmpty, singleton, lookup, alter, insert, delete, update) where

import Data.Word ( Word8 )
import Data.Maybe (isNothing, fromMaybe)
import Data.Bifunctor (bimap)
import qualified Data.List as L
import Concordium.GlobalState.Persistent.BlobStore (BufferedRef, makeBufferedRef, BlobStorable (storeUpdate, store, load), loadBufferedRef)
import Control.Monad.IO.Class ( MonadIO )
import Data.Serialize (getWord8, Get, Put, putWord8)
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import Data.Vector (unsafeTail, (!?))
import qualified Data.ByteString.Unsafe as BS.Unsafe
import Prelude hiding (lookup)

type Prefix = BS.ByteString
type Key = BS.ByteString

-- | A branch in a tree node, it consists of prefix byte for the branch and a reference to the node for this branch.
type Branch a = (Word8, BufferedRef (Trie a))

-- | Compressed trie with keys fixed as bytestrings.
data Trie a = Trie {
                -- | A compressed prefix of this node and the branches.
                prefix :: Prefix,
                -- | The value in this node.
                 value :: Maybe a,
                -- | The branches from this node which are expected to be sorted.
                branches :: V.Vector (Branch a) }
  deriving (Show)

instance (BlobStorable m a) => BlobStorable m (Trie a) where
  store node = fst <$> storeUpdate node
  storeUpdate node = do
    (putPrefix, p) <- storeUpdate (prefix node)
    -- Store the value if any
    (putValue, v) <- storeUpdateValue
    -- Store the branches
    (putBranches, bs) <- storeUpdateBranches
    -- Construct the storeUpdatedNode
    let putNode = putPrefix >> putValue >> putBranches
    let updatedNode = Trie {prefix = p, value = v, branches = bs}
    return (putNode, updatedNode)
    where
      storeUpdateValue :: m (Put, Maybe a)
      storeUpdateValue = do
        case value node of
          Just v -> bimap (putWord8 1 <>) Just <$> storeUpdate v
          Nothing -> return (putWord8 0, Nothing)

      -- | storeUpdate for a single branch of the node
      storeUpdateBranch :: Branch a -> m (Put, Branch a)
      storeUpdateBranch (branchPrefix, ref) = do
        (refPut, updatedRef) <- storeUpdate ref
        let putBranch = putWord8 branchPrefix >> refPut
        let updatedBranch = (branchPrefix, updatedRef)
        return (putBranch, updatedBranch)

      storeUpdateBranches :: m (Put, V.Vector (Branch a))
      storeUpdateBranches = do
        let bs = branches node
        let branchLength = putWord8 $ fromIntegral $ L.length bs
        storeBs <- mapM storeUpdateBranch bs
        let putBranches = branchLength >> sequence_ (fst <$> storeBs)
        return (putBranches , snd <$> storeBs)

  load = do
    p <- load
    -- Load the nodes value if any
    v <- loadValue
    -- Load branches
    bs <- loadBranches
    -- Construct the node
    return $ do p' <- p
                v' <- v
                bs' <- bs
                return Trie {prefix = p', value = v', branches = bs'}
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
        branchPrefix <- getWord8
        ref :: m (BufferedRef (Trie a)) <- load
        return $ (branchPrefix, ) <$> ref

      loadBranches :: Get (m (V.Vector (Branch a)))
      loadBranches = do
        branchLength <- fromIntegral <$> getWord8
        sequence <$> V.replicateM branchLength loadBranch

-- | A trie containing no keys.
empty :: Trie a
empty = Trie { prefix = BS.empty, value = Nothing, branches = V.empty }

-- | Check if the trie contains any values.
-- Assumes the trie is compressed.
isEmpty :: Trie a -> Bool
isEmpty trie = isNothing (value trie) && V.null (branches trie)

-- | Construct a trie with key and value.
singleton :: Prefix -> a -> Trie a
singleton prefix value = Trie {prefix = prefix, value = Just value, branches = V.empty}

-- | Construct trie node with given prefix and branches and no value.
makeBranchNode :: Prefix -> V.Vector (Branch a) -> Trie a
makeBranchNode prefix branches = Trie{prefix = prefix, value = Nothing, branches = branches}

-- | Construct branch with a given byte and trie node.
makeBranch :: MonadIO m => Word8 -> Trie a -> m (Branch a)
makeBranch prefix node = do bnode <- makeBufferedRef node
                            return (prefix, bnode)

-- | Take the head and tail of a vector.
unconsVec :: V.Vector a -> Maybe (a, V.Vector a)
unconsVec xs = (, unsafeTail xs) <$> (xs !? 0)

-- | Find the shared prefix between two ByteStrings and return the shared prefix plus what remains of each bytestring.
sharedPrefixOf :: Prefix -> Prefix -> (Prefix, Prefix, Prefix)
sharedPrefixOf keyL keyR =
  let sharedUntil = fromMaybe (min (BS.length keyL) (BS.length keyR)) $ L.findIndex (uncurry (/=)) (BS.zip keyL keyR)
      shared = BS.Unsafe.unsafeTake sharedUntil keyL -- Does not check bounds
      remainingKeyL = BS.Unsafe.unsafeDrop sharedUntil keyL -- Does not check bounds
      remainingKeyR = BS.Unsafe.unsafeDrop sharedUntil keyR -- Does not check bounds
  in (shared, remainingKeyL, remainingKeyR)

-- | Lookup a givent key in a trie.
-- Assumes each node have branches sorted by their prefix byte, likewise it ensures inserted branches follow this.
lookup :: BlobStorable m a => Key -> Trie a -> m (Maybe a)
lookup key node =
  -- If the key is empty the value is in the current node
  let (_, strippedPrefix, strippedKey) = prefix node `sharedPrefixOf` key in
  -- If something remains from the node prefix the node is not in the tree
  if not $ BS.null strippedPrefix then
    return Nothing
  -- If the key is empty, then this must be the node
  else if BS.null strippedKey then
      return $ value node
  -- Otherwise lookup in branches
  else
    let nextNode = do (byte, remainingKey) <- BS.uncons strippedKey
                      branch <- V.find ((== byte) . fst) (branches node)
                      return (remainingKey, snd branch)
    in case nextNode of
       Just (remainingKey, nodeRef) -> loadBufferedRef nodeRef >>= lookup remainingKey
       Nothing -> return Nothing

-- | A generalized update function which can be used to insert, delete and update values of the map given some key.
-- The update function will receive the current value (Just a) or Nothing if the value is currently not in the map.
-- If the result of the function will replace the value, if the result is Nothing the key will be removed.
--
-- It will ensure to compress branches when inserting and removing keys.
--
-- Assumes each node have branches sorted by their prefix byte, likewise it ensures inserted branches follow this.
alter :: BlobStorable m a => (Maybe a -> Maybe a) -> Key -> Trie a -> m (Trie a)
alter alterFn key node =
  if isEmpty node then
    whenInsertValue $ return . singleton key
  else
    let (sharedPrefix, strippedPrefix, strippedKey) = prefix node `sharedPrefixOf` key in
    case BS.uncons strippedKey of
        -- If the key is empty, we are at the node with the value
        Nothing -> return node { value = alterFn (value node) }
        -- Using the first byte of the key we search and split the node branches right before the branch with a byte which matches or is higher.
        Just (byte, remainingKey) ->
          -- Node prefix is not a complete prefix of the key, so the prefix must be split and a new node must be made if inserting
          case BS.uncons strippedPrefix of
            Just (remainingPrefixByte, remainingStrippedPrefix) ->
              whenInsertValue (\value ->
                                 do updatedBranch <- makeBranch remainingPrefixByte $ node {prefix = remainingStrippedPrefix}
                                    newValueBranch <- makeBranch byte $ singleton remainingKey value
                                    let splitBranches = V.fromList $ if byte < remainingPrefixByte then [newValueBranch, updatedBranch] else [updatedBranch, newValueBranch]
                                    return $ makeBranchNode sharedPrefix splitBranches)
          -- Node prefix is a complete prefix of the key
            Nothing ->

              let (lessBranches, greaterOrEq) = V.span ((< byte) . fst) (branches node) in
                case unconsVec greaterOrEq of
                  -- No branch containing byte or higher, so the key is not in the map.
                  Nothing -> whenInsertValue (\value -> do branch <- makeBranch byte $ singleton remainingKey value
                                                           return $ node {branches = V.snoc lessBranches branch })
                  -- Exists branch with byte equal or higher
                  Just (maybeEqBranch, greaterBranches) ->
                    if fst maybeEqBranch /= byte then
                      -- No branch contains the next key byte, so we insert a new one
                      whenInsertValue (\value -> do newBranch <- makeBranch byte $ singleton remainingKey value
                                                    return node { branches = V.concat [lessBranches, V.singleton newBranch, greaterOrEq] })
                    else
                      -- We found a branch containing the byte, so we recurse and ensure to compress if needed
                      do branchNode <- loadBufferedRef $ snd maybeEqBranch
                         updatedBranchNode <- alter alterFn remainingKey branchNode

                         -- If the updated branch is now empty we remove it.
                         if isEmpty updatedBranchNode then
                           return node {branches = lessBranches V.++ greaterBranches}
                           -- The updated branch have only a single branch and no value, we load this branch, update the prefix and replace the updatedbranch
                         else if isSingleBranchNode updatedBranchNode && isNothing (value updatedBranchNode) then
                           do let updatedSubBranch = V.head $ branches updatedBranchNode
                              subBranchNode <- loadBufferedRef $ snd updatedSubBranch
                              let subBranchNodeWithPrefix = subBranchNode { prefix = BS.append (prefix updatedBranchNode) (prefix subBranchNode) }
                              updatedBranch <- makeBranch byte subBranchNodeWithPrefix
                              return node {branches = V.concat [lessBranches, V.singleton updatedBranch, greaterBranches]}
                         -- Otherwise we simply replace the branch with the updated one.
                         else
                           do updatedBranch <- makeBranch byte updatedBranchNode
                              return node {branches = V.concat [lessBranches, V.singleton updatedBranch, greaterBranches]}
  where
    -- | Run the alterFn with Nothing and return the node unmodified if the result is Nothing otherwise it calls the provided function with the result.
    -- Use only when the trie does not contain the key
    whenInsertValue insertFn = case alterFn Nothing of
       Nothing -> return node
       Just value -> insertFn value

    -- | Check if the node contains only a single branch.
    isSingleBranchNode :: Trie a -> Bool
    isSingleBranchNode trie = 1 == V.length (branches trie)

-- | Insert new key and value into the trie.
insert ::  BlobStorable m a => Key -> a -> Trie a -> m (Trie a)
insert key value = alter (const (Just value)) key

-- | Delete a key from the trie, will not modify the trie if the key is not in the trie.
delete :: BlobStorable m a => Key -> Trie a -> m (Trie a)
delete = alter (const Nothing)

-- | Update a value at a given key, will not modify the trie if the key is not in the trie.
update :: BlobStorable m a => (a -> Maybe a) -> Key -> Trie a -> m (Trie a)
update upd = alter (>>= upd)
