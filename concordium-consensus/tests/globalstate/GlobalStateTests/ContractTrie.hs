{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
--{-# OPTIONS_GHC -Wno-deprecations #-}
module GlobalStateTests.ContractTrie where

import Control.Monad.IO.Class
import Data.Serialize
import qualified Data.ByteString as BS
import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types.HashableTo (MHashableTo, getHashM)

-- import Test.QuickCheck
import Test.Hspec

import Concordium.GlobalState.Persistent.BlobStore (runBlobStoreTemp, MonadBlobStore, BlobStorable, storeUpdate, load)
import qualified Concordium.GlobalState.Persistent.ContractTrie as T

-- | Newtype providing a @BlobStorable@ reference for every wrapped type
--  that is an instance of @Serialize@
newtype SerializeStorable v = SerStore v
  deriving newtype (Eq, Ord, Show, Serialize)

-- Every @SerializeStorable@ value will be serialized with the default implementation
instance (Serialize v, MonadBlobStore m) => BlobStorable m (SerializeStorable v)

instance (Serialize v, MonadBlobStore m) => MHashableTo m H.Hash (SerializeStorable v) where
  getHashM v = return $ H.hash $ runPut (put v)

tests :: Spec
tests = describe "GlobalStateTests.ContractTrie" $ do
  it "should find something when lookup an inserted key" $
    runBlobStoreTemp "." $ do
      let key1 = BS.pack [1,2,27]
      let key2 = BS.pack [1,3,27]
      let value1 = SerStore "Hello"
      let value2 = SerStore "World"
      let e = T.empty
      e0 <- T.insert key1 value1 e
      e1 <- T.insert key2 value2 e0
      r <- T.lookup key1 e1
      liftIO $ r `shouldBe` Just value1

  it "should not find something when looking up key not inserted" $
    runBlobStoreTemp "." $ do
      let key1 = BS.pack [1,2,27]
      let key2 = BS.pack [1,3,27]
      let value1 = SerStore "Hello"
      let e = T.empty
      e0 <- T.insert key1 value1 e
      r <- T.lookup key2 e0
      liftIO $ r `shouldBe` Nothing

  it "should not find something key when deleted" $
    runBlobStoreTemp "." $ do
      let key1 = BS.pack [1,2,27]
      let key2 = BS.pack [1,3,27]
      let value1 = SerStore "Hello"
      let value2 = SerStore "World"
      let e = T.empty
      e0 <- T.insert key1 value1 e
      e1 <- T.insert key2 value2 e0
      e2 <- T.delete key2 e1
      r <- T.lookup key2 e2
      liftIO $ r `shouldBe` Nothing

  it "should find inserted values after storing and loading on disc" $
    runBlobStoreTemp "." $ do
      let key1 = BS.pack [1,2,27]
      let key2 = BS.pack [1,3,27]
      let value1 = SerStore "Hello"
      let value2 = SerStore "World"
      let e = T.empty
      e0 <- T.insert key1 value1 e
      e1 <- T.insert key2 value2 e0
      (e1Put, e1Updated) <- storeUpdate e1
      rUpdated <- T.lookup key1 e1Updated
      let (Right e1Load) = runGet load (runPut e1Put)
      e1Loaded <- e1Load
      rLoaded <- T.lookup key1 e1Loaded
      liftIO $ do rUpdated `shouldBe` Just value1
                  rLoaded `shouldBe` Just value1

  it "should hash to the same for different insertion order" $
    runBlobStoreTemp "." $ do
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
    runBlobStoreTemp "." $ do
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
