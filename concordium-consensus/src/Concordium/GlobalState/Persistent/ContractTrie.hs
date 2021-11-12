{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Concordium.GlobalState.Persistent.ContractTrie (Trie, Key, empty, isEmpty, singleton, lookup, alter, insert, delete, update, fromList, borrowTrie) where

import Data.Word (Word8)
import Data.Maybe (isNothing, fromMaybe)
import Data.Bifunctor (bimap)
import qualified Data.List as L
import Concordium.GlobalState.Persistent.BlobStore (BlobStorable (storeUpdate, store, load), HashedBufferedRef, makeHashedBufferedRef, refLoad, MonadBlobStore, runBlobStoreTemp)
import Data.Serialize (getWord8, Get, Put, putWord8, Serialize, put)
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS.Unsafe
import Prelude hiding (lookup)
import Concordium.Types.HashableTo (MHashableTo, getHashM)
import qualified Concordium.Crypto.SHA256 as H
import Data.Foldable (foldlM)
import Data.IORef ( readIORef, IORef, newIORef, writeIORef )
import Control.Monad.IO.Class (liftIO)
import Data.Functor (($>))
import Data.Serialize.Put (runPut)
import System.IO.Unsafe (unsafePerformIO)

type Prefix = BS.ByteString
type Key = BS.ByteString

-- TODO: Consider adding hashedBufferedRef only to borrowed, to avoid calculating the hash and making a ref until ownership is lost.
data Cow a = Borrowed a
           | Owned (IORef a)

instance (Show a) => Show (Cow a) where
  show cow = case cow of
    Borrowed a -> "Borrowed " ++ show a
    Owned ref -> "Owned " ++ (show $ unsafePerformIO $ readIORef ref)

instance (BlobStorable m a, MHashableTo m H.Hash a) => BlobStorable m (Cow a) where
  store cow = fst <$> storeUpdate cow
  storeUpdate cow = do
    v <- liftIO $ readCow cow
    (putV, v') <- storeUpdate v
    return (putV, newBorrowedCow v')
  load = do getV <- load
            return $ newBorrowedCow <$> getV

newBorrowedCow :: a -> Cow a
newBorrowedCow = Borrowed

newOwnedCow :: a -> IO (Cow a)
newOwnedCow value = Owned <$> newIORef value

isBorrowedCow :: Cow a -> Bool
isBorrowedCow cow = case cow of
  Borrowed _ -> True
  _ -> False

writeCow :: a -> Cow a -> IO (Maybe (Cow a))
writeCow value cow = case cow of
  Owned ref -> writeIORef ref value $> Nothing
  Borrowed _ -> Just . Owned <$> newIORef value

readCow :: Cow a -> IO a
readCow cow = case cow of
  Owned ref -> readIORef ref
  Borrowed a -> return a

-- | A branch in a tree node, it consists of prefix byte for the branch and a reference to the node for this branch.
type Branch a = (Word8, Cow (HashedBufferedRef (Trie a)))

-- | Compressed trie with keys fixed as bytestrings.
data Trie a = Trie {
  -- | A compressed prefix of this node and the branches.
  prefix :: Prefix,
  -- | The value in this node.
  value :: Maybe a,
  -- | The branches from this node which are expected to be sorted.
  branches :: V.Vector (Branch a)
  } deriving (Show)

instance (BlobStorable m a, MHashableTo m H.Hash a) => MHashableTo m H.Hash (Trie a) where
  getHashM node = do
    let prefixBytes = prefix node
    valueHashBytes <- case value node of
      Nothing -> return "N"
      Just v -> BS.append "J" . H.hashToByteString <$> getHashM v
    branchHashes <- sequence $ hashBranch <$> V.toList (branches node)
    return $ H.hash $ BS.concat $ prefixBytes : valueHashBytes : (H.hashToByteString <$> branchHashes)
    where
      hashBranch :: Branch a -> m H.Hash
      hashBranch branch = do
        ref <- liftIO $ readCow (snd branch)
        getHashM ref

instance (BlobStorable m a, MHashableTo m H.Hash a) => BlobStorable m (Trie a) where
  store node = fst <$> storeUpdate node
  storeUpdate node = do
    -- Store the prefix of the node
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

      storeUpdateBranch :: Branch a -> m (Put, Branch a)
      storeUpdateBranch branch = do
        (refPut, updatedRef) <- storeUpdate (snd branch)
        let putBranch = putWord8 (fst branch) >> refPut
        let updatedBranch = (fst branch, updatedRef)
        return (putBranch, updatedBranch)

      storeUpdateBranches :: m (Put, V.Vector (Branch a))
      storeUpdateBranches = do
        let bs = branches node
        let branchLength = putWord8 $ fromIntegral $ L.length bs
        storeBs <- mapM storeUpdateBranch bs
        let putBranches = branchLength >> sequence_ (fst <$> storeBs)
        return (putBranches , snd <$> storeBs)

  load = do
    -- Load the prefix of the node
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
        ref <- load
        return $ (branchPrefix, ) <$> ref

      loadBranches :: Get (m (V.Vector (Branch a)))
      loadBranches = do
        branchLength <- fromIntegral <$> getWord8
        sequence <$> V.replicateM branchLength loadBranch

-- | A trie containing no keys or values.
empty :: Trie a
empty = Trie { prefix = BS.empty, value = Nothing, branches = V.empty }

-- | Returns true if the trie does not contain a value or any branches.
-- Assumes the trie is compressed.
isEmpty :: Trie a -> Bool
isEmpty trie = V.null (branches trie) && isNothing (value trie)

-- | Construct a trie with key and value.
singleton :: Prefix -> a -> Trie a
singleton prefix value = Trie { prefix = prefix, value = Just value, branches = V.empty }

-- | Construct trie node with given prefix and branches and no value.
makeBranchNode :: Prefix -> V.Vector (Branch a) -> Trie a
makeBranchNode prefix branches = Trie { prefix = prefix, value = Nothing, branches = branches }

-- | Construct branch with a given byte and trie node.
makeBranch :: (BlobStorable m a, MHashableTo m H.Hash a) => Word8 -> Trie a -> m (Branch a)
makeBranch prefix node = do bnode <- makeHashedBufferedRef node
                            cow <- liftIO $ newOwnedCow bnode
                            return (prefix, cow)

-- | Checks if a node only have borrowed branches, which in turn means it must copy before mutating.
isBorrowedNode :: Trie a -> Bool
isBorrowedNode node = V.all (isBorrowedCow . snd) $ branches node

-- | Load the node reference in a branch.
readBranch :: (BlobStorable m a, MHashableTo m H.Hash a) => Branch a -> m (Trie a)
readBranch (_, cow) = liftIO (readCow cow) >>= refLoad

-- | Take the head and tail of a vector.
unconsVec :: V.Vector a -> Maybe (a, V.Vector a)
unconsVec xs = (, V.unsafeTail xs) <$> (xs V.!? 0)

-- | Find the shared prefix between two ByteStrings and return the shared prefix plus what remains of each bytestring.
sharedPrefixOf :: Prefix -> Prefix -> (Prefix, Prefix, Prefix)
sharedPrefixOf keyL keyR =
  let sharedUntil = fromMaybe (min (BS.length keyL) (BS.length keyR)) $ L.findIndex (uncurry (/=)) (BS.zip keyL keyR)
      shared = BS.Unsafe.unsafeTake sharedUntil keyL -- Does not check bounds
      remainingKeyL = BS.Unsafe.unsafeDrop sharedUntil keyL -- Does not check bounds
      remainingKeyR = BS.Unsafe.unsafeDrop sharedUntil keyR -- Does not check bounds
  in (shared, remainingKeyL, remainingKeyR)

-- | Lookup a given key in a trie.
-- Assumes each node have branches sorted by their prefix byte, likewise it ensures inserted branches follow this.
lookup :: (BlobStorable m a, MHashableTo m H.Hash a) => Key -> Trie a -> m (Maybe a)
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
                      return (remainingKey, branch)
    in case nextNode of
       Just (remainingKey, branch) -> readBranch branch >>= lookup remainingKey
       Nothing -> return Nothing

-- | A generalized update function which can be used to insert, delete and update values of the map given some key.
-- The update function will receive the current value (Just a) or Nothing if the value is currently not in the map.
-- If the result of the function will replace the value, if the result is Nothing the key will be removed.
--
-- It will ensure to compress branches when inserting and removing keys.
--
-- Assumes each node have branches sorted by their prefix byte, likewise it ensures inserted branches follow this.
alter :: (BlobStorable m a, MHashableTo m H.Hash a) => (Maybe a -> Maybe a) -> Key -> Trie a -> m (Trie a)
alter alterFn key node =
  if isEmpty node then
    whenInsertValue $ return . singleton key
  else
    let (sharedPrefix, strippedPrefix, strippedKey) = prefix node `sharedPrefixOf` key in
    case BS.uncons strippedKey of
        -- If the key is empty, we are at the node with the value
        Nothing -> return node { value = alterFn (value node) }
        Just (byte, remainingKey) ->
          -- Node prefix is not a complete prefix of the key, so the prefix must be split and a new node must be made if inserting
          case BS.uncons strippedPrefix of
            Just (remainingPrefixByte, remainingStrippedPrefix) ->
              whenInsertValue (\value ->
                                 do updatedBranch <- makeBranch remainingPrefixByte $ node {prefix = remainingStrippedPrefix}
                                    let newNode = singleton remainingKey value
                                    newValueBranch <- makeBranch byte newNode
                                    let splitBranches = V.fromList $ if byte < remainingPrefixByte then [newValueBranch, updatedBranch] else [updatedBranch, newValueBranch]
                                    return $ makeBranchNode sharedPrefix splitBranches)
          -- Node prefix is a complete prefix of the key
            Nothing ->

              let (lessBranches, greaterOrEq) = V.span ((< byte) . fst) (branches node) in
                case unconsVec greaterOrEq of
                  -- No branch containing byte or higher, so the key is not in the map.
                  Nothing -> whenInsertValue (\value -> do let branchNode = singleton remainingKey value
                                                           branch <- makeBranch byte branchNode
                                                           return $ node {branches = V.snoc lessBranches branch })
                  -- Exists branch with byte equal or higher
                  Just (maybeEqBranch, greaterBranches) ->
                    if fst maybeEqBranch /= byte then
                      -- No branch contains the next key byte, so we insert a new one
                      whenInsertValue (\value -> do let branchNode = singleton remainingKey value
                                                    newBranch <- makeBranch byte branchNode
                                                    return node { branches = V.concat [lessBranches, V.singleton newBranch, greaterOrEq] })
                    else
                      -- We found a branch containing the byte, so we recurse and ensure to compress if needed
                      do branchNode <- readBranch maybeEqBranch
                         updatedBranchNode <- alter alterFn remainingKey branchNode

                         -- If the updated branch is now empty we remove it.
                         if isEmpty updatedBranchNode then
                            -- If removing the branch will result in a having 1 branch and no value, we compress by returning the only branch
                            if V.length (branches node) == 2 && isNothing (value node) then
                              do let onlyBranch =  V.head $ if V.length lessBranches == 1 then lessBranches else greaterBranches
                                 onlyBranchNode <- readBranch onlyBranch
                                 let newPrefix = BS.append (prefix node) (BS.cons (fst onlyBranch) (prefix onlyBranchNode))
                                 return onlyBranchNode { prefix = newPrefix }
                            -- Otherwise we just remove the branch
                            else
                              return node { branches = lessBranches V.++ greaterBranches }
                         -- If the updateBranchNode is not empty we replace the branch.
                         else
                           do updatedBranch <- writeBranch maybeEqBranch updatedBranchNode
                              case updatedBranch of
                                Just newBranch -> return node { branches = V.concat [lessBranches, V.singleton newBranch, greaterBranches] }
                                Nothing -> return node
  where
    -- | Run the alterFn with Nothing and return the node unmodified if the result is Nothing otherwise it calls the provided function with the result.
    -- Use only when the trie does not contain the key.
    whenInsertValue insertFn = case alterFn Nothing of
       Nothing -> return node
       Just value -> insertFn value

    -- | Update the node in a branch, copying it if necessary.
    writeBranch :: (BlobStorable m a, MHashableTo m H.Hash a) => Branch a -> Trie a -> m (Maybe (Branch a))
    writeBranch (byte, cow) newBranchNode = do
      nodeRef <- makeHashedBufferedRef newBranchNode
      maybeNewBranch <- liftIO $ writeCow nodeRef cow
      return $ (byte,) <$> maybeNewBranch

-- | Insert new key and value into the trie.
insert ::  (BlobStorable m a, MHashableTo m H.Hash a) => Key -> a -> Trie a -> m (Trie a)
insert key value = alter (const (Just value)) key

-- | Delete a key from the trie, will not modify the trie if the key is not in the trie.
delete :: (BlobStorable m a, MHashableTo m H.Hash a) => Key -> Trie a -> m (Trie a)
delete = alter (const Nothing)

-- | Update a value at a given key, will not modify the trie if the key is not in the trie.
update :: (BlobStorable m a, MHashableTo m H.Hash a) => (a -> Maybe a) -> Key -> Trie a -> m (Trie a)
update upd = alter (>>= upd)

-- | Construct a trie from a list of key-value pairs.
fromList :: (BlobStorable m a, MHashableTo m H.Hash a) => [(Key, a)] -> m (Trie a)
fromList = foldlM (flip $ uncurry insert) empty

-- | Construct a trie not owning any of its nodes.
-- Assumes a node with only borrowed branches only contains borrowed nodes.
borrowTrie :: (BlobStorable m a, MHashableTo m H.Hash a) => Trie a -> m (Trie a)
borrowTrie node =
  if isBorrowedNode node then
    return node
  else
    do newBranches <- V.mapM borrowBranch $ branches node
       return node { branches = newBranches }
  where
    borrowBranch :: (BlobStorable m a, MHashableTo m H.Hash a) => Branch a -> m (Branch a)
    borrowBranch (byte, cow) = do
      branchNode <- liftIO (readCow cow) >>= refLoad
      borrowedNode <- borrowTrie branchNode
      newCow <- newBorrowedCow <$> makeHashedBufferedRef borrowedNode
      return (byte, newCow)

