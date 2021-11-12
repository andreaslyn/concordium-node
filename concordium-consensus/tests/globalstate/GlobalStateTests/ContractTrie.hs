{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
--{-# OPTIONS_GHC -Wno-deprecations #-}
module GlobalStateTests.ContractTrie where

import Control.Monad.IO.Class
import Data.Serialize
import qualified Data.ByteString as BS
import qualified Concordium.Crypto.SHA256 as H
import Data.Bifunctor (second)
import Concordium.Types.HashableTo (MHashableTo, getHashM)

-- import Test.QuickCheck
import Test.Hspec

import Concordium.GlobalState.Persistent.BlobStore (runBlobStoreTemp, MonadBlobStore, BlobStorable, storeUpdate, load)
import qualified Concordium.GlobalState.Persistent.ContractTrie as T

-- | Newtype providing a @BlobStorable@ reference for every wrapped type
--  that is an instance of @Serialize@
newtype SerializeStorable = SerStore String
  deriving newtype (Eq, Ord, Show, Serialize)

-- Every @SerializeStorable@ value will be serialized with the default implementation
instance (MonadBlobStore m) => BlobStorable m SerializeStorable

instance (MonadBlobStore m) => MHashableTo m H.Hash SerializeStorable where
  getHashM v = return $ H.hash $ runPut (put v)

run = runBlobStoreTemp "."

tests :: Spec
tests = describe "GlobalStateTests.ContractTrie" $ do
  it "isEmpty is true for empty" $ let
      r = T.isEmpty T.empty
    in r `shouldBe` True

  it "isEmpty is false for singleton" $ let
      r = T.isEmpty $ T.singleton "abc" $ SerStore "hello"
    in r `shouldBe` False

  it "should find something when lookup an inserted key" $
    run $ do
      let key1 = BS.pack [1,2,27]
      let key2 = BS.pack [1,3,27]
      let value1 = SerStore "Hello"
      let value2 = SerStore "World"
      e0 <- T.insert key1 value1 T.empty
      e1 <- T.insert key2 value2 e0
      r <- T.lookup key1 e1
      liftIO $ r `shouldBe` Just value1

  it "should not find something when looking up key not inserted" $
    run $ do
      let key1 = BS.pack [1,2,27]
      let key2 = BS.pack [1,3,27]
      let value1 = SerStore "Hello"
      e0 <- T.insert key1 value1 T.empty
      r <- T.lookup key2 e0
      liftIO $ r `shouldBe` Nothing

  it "should not find something key when deleted" $
    run $ do
      let key1 = BS.pack [1,2,27]
      let key2 = BS.pack [1,3,27]
      let value1 = SerStore "Hello"
      let value2 = SerStore "World"
      e0 <- T.insert key1 value1 T.empty
      e1 <- T.insert key2 value2 e0
      e2 <- T.delete key2 e1
      r <- T.lookup key2 e2
      liftIO $ r `shouldBe` Nothing

  it "should find inserted values after storing and loading on disc" $
    run $ do
      let key1 = BS.pack [1,2,27]
      let key2 = BS.pack [1,3,27]
      let value1 = SerStore "Hello"
      let value2 = SerStore "World"
      e0 <- T.insert key1 value1 T.empty
      e1 <- T.insert key2 value2 e0
      (e1Put, e1Updated) <- storeUpdate e1
      rUpdated <- T.lookup key1 e1Updated
      let (Right e1Load) = runGet load (runPut e1Put)
      e1Loaded <- e1Load
      rLoaded <- T.lookup key1 e1Loaded
      liftIO $ do rUpdated `shouldBe` Just value1
                  rLoaded `shouldBe` Just value1

  it "should hash to the same for different insertion order" $
    run $ do
      let key1 = BS.pack [1,2,27]
      let key2 = BS.pack [1,3,27]
      let value1 = SerStore "Hello"
      let value2 = SerStore "World"
      t1 <- T.fromList [(key1, value1), (key2, value2)]
      t2 <- T.fromList [(key2, value2), (key1, value1)]
      h1 :: H.Hash <- getHashM t1
      h2 <- getHashM t2
      liftIO $ h1 `shouldBe` h2

  it "should hash to the same after inserting and deleting a key" $
    run $ do
      let key1 = BS.pack [1,2,27]
      let key2 = BS.pack [1,3,27]
      let value1 = SerStore "Hello"
      let value2 = SerStore "World"
      let t1 = T.singleton key1 value1
      t2 <- T.insert key2 value2 t1
      t1' <- T.delete key2 t2
      h1 :: H.Hash <- getHashM t1
      h2 <- getHashM t1'
      liftIO $ h1 `shouldBe` h2

  it "should mutate the trie, when owning the nodes" $
    run $ do
      let v1 = SerStore "X"
      let v2 = SerStore "Y"
      t1 <- T.fromList $ fmap (second SerStore) [("abc", "A"), ("abd", "B"), ("ag", "C")]
      -- since we own the nodes, inserting should mutate the node.
      T.insert "abd" v1 t1
      r1 <- T.lookup "abd" t1
      liftIO $ r1 `shouldBe` Just v1
      t1' <- T.borrowTrie t1
      -- Since we do not own the any of the nodes anymore: new inserts should not mutate the trie.
      t2 <- T.insert "abd" v2 t1'
      r1' <- T.lookup "abd" t1'
      liftIO $ r1' `shouldBe` Just v1
      r2 <- T.lookup "abd" t2
      liftIO $ r2 `shouldBe` Just v2

