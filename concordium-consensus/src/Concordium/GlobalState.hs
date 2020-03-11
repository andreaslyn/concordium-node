{-# LANGUAGE
    GeneralizedNewtypeDeriving,
    StandaloneDeriving,
    DerivingVia,
    FlexibleContexts,
    FlexibleInstances,
    DerivingStrategies,
    TypeFamilies,
    RecordWildCards,
    MultiParamTypeClasses,
    QuantifiedConstraints,
    UndecidableInstances,
    CPP,
    RankNTypes,
    ScopedTypeVariables,
    ConstraintKinds,
    PartialTypeSignatures
    #-}
-- |This module lifts the abstractions declared in the globalstate package to an
-- abstracted new type `GlobalStateM` that inherits all the monad behaviors defined
-- in this package.
module Concordium.GlobalState where

import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Reader
import Data.IORef (newIORef,writeIORef)
import Data.Proxy
import Data.Serialize.Put (runPut)
import Data.ByteString.Char8(ByteString)
import System.FilePath
import Data.Pool(destroyAllResources)

import Concordium.Types.Transactions
import Concordium.GlobalState.Basic.BlockState as BS
import Concordium.GlobalState.Basic.TreeState
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Classes as GS
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Persistent.BlobStore (createTempBlobStore,destroyTempBlobStore)
import Concordium.GlobalState.Persistent.BlockState
import Concordium.GlobalState.Persistent.BlockState (PersistentBlockStateContext(..), PersistentBlockStateMonad, PersistentBlockState)
import qualified Concordium.GlobalState.Persistent.BlockState as Persistent
import Concordium.GlobalState.Persistent.TreeState
import Concordium.GlobalState.TreeState as TS
import Concordium.Types.PersistentTransactions
import Concordium.GlobalState.AccountTransactionIndex
import Concordium.GlobalState.SQLiteATI

-- For the avid reader.
-- The strategy followed in this module is the following: First `BlockStateM` and
-- `TreeStateM` derive the behaviors of the BlockState Monads and TreeState Monads
-- respectively. If such behavior is independent of the specific implementation of
-- the monad we use generic implementations. As these are just wrappers that lift
-- behaviors, some of these are generic.
--
-- For example, deriving `MonadIO` when wrapping a monad that is already an instance
-- of `MonadIO` is trivially done in the deriving clause next to the definition of
-- the new type. Deriving a class that is agnostic to the types used (usually because
-- the relations between those types are given by another typeclass) is done in the
-- `Generic implementations` section for each type.
--
-- When the actual typeclass that is being derived depends on the type variables to
-- inherit one behavior or another (for example with the `TreeStateMonad` implementation)
-- we actually need to use two derive instances to generate the different type classes.
-- These derivations are done in the `Specialized implementations` section for each type.
--
-- The general strategy followed for these derivations is:
--
-- 1. derive via a type that is already an instance of the class we want (for example
-- `PureTreeStateMonad` is an instance of `TreeStateMonad`).
-- 2. Use the constraint `Monad m` because the newtype we are mirroring needs this to
-- even exist (`Monad m => MonadIO/MonadState s/MonadReader r/MonadWriter w m` so any
-- of those also work).
-- 3. If we are deriving the class `C` via the type `T`, require the constraint `C T`
-- that we know is true for some instances of the type variables in `T`.
-- 3b. If the class `C` depends in another typeclass `D` that is not implemented in a
-- generic way, we will need to add this constraint to our derivation. This happens
-- with the `BlockStateStorage m` constraint when deriving the `TreeStateMonad`,
-- as `BlockStateStorage` is not mandatory for all the possible instantiations of
-- the type variables in the `T` type.
-- 4. Declare the instance `C V` being V the actual newtype we want to give this
-- behavior.
--
-- In the case of the `GlobalStateM` new type we will derive the BlockState monads,
-- generically from `BlockStateM` and the TreeState monads also generically from
-- `TreeStateM bs (BlockStateM ..)`.


-- |A newtype wrapper for providing instances of the block state related monads:
-- 'BlockStateTypes', 'BlockStateQuery', 'BlockStateOperations' and 'BlockStateStorage'.
--
-- For the monad @BlockStateM c r g s m@, the underlying monad @m@ should satisfy
-- @MonadReader r m@ and @MonadState s m@.  The types @c@ and @s@ should be components
-- of the context @r@ and state @s@, satisfying @HasGlobalStateContext c r@ and
-- @HasGlobalState g s@ respectively.
--
-- The particular instances for the block state monads are determined by the @c@ and @g@
-- parameters (although currently @g@ is not used).
--
-- * If @c@ is @()@, the block state is a pure, in-memory, Haskell implementation using
--   'Basic.PureBlockStateMonad'.
--
-- * If @c@ is 'PersistentBlockStateContext', the block state is a persistent, Haskell
--   implementation using 'PersistentBlockStateMonad'.
newtype BlockStateM c r g s m a = BlockStateM (m a)
    deriving (Functor, Applicative, Monad, MonadIO)

-- * Specializations

type MB r g s m = BlockStateM () r g s m
type DB r g s m = BlockStateM PersistentBlockStateContext r g s m

-- * Generic implementations

deriving via FocusGlobalStateM c g m
    instance (HasGlobalStateContext c r, MonadReader r m)
             => MonadReader c (BlockStateM c r g s m)

deriving via FocusGlobalStateM c g m
    instance (HasGlobalState g s, MonadState s m)
             => MonadState g (BlockStateM c r g s m)

-- * Specific implementations

-- ** Memory implementations
deriving via PureBlockStateMonad m
    instance BlockStateTypes (MB r g s m)

deriving via PureBlockStateMonad m
    instance (Monad m)
             => BlockStateQuery (MB r g s m)

deriving via PureBlockStateMonad m
    instance (Monad m,
              BlockStateQuery (MB r g s m))
             => BlockStateOperations (MB r g s m)

deriving via PureBlockStateMonad m
    instance (Monad m,
              BlockStateStorage (PureBlockStateMonad m))
             => BlockStateStorage (MB r g s m)

-- ** Disk implementations
deriving via (PersistentBlockStateMonad PersistentBlockStateContext m)
    instance BlockStateTypes (DB r g s m)

deriving via (PersistentBlockStateMonad
               PersistentBlockStateContext
               (FocusGlobalStateM PersistentBlockStateContext g m))
    instance (MonadIO m,
              BlockStateQuery (PersistentBlockStateMonad
                                PersistentBlockStateContext
                                (FocusGlobalStateM PersistentBlockStateContext g m)))
             => BlockStateQuery (DB r g s m)

deriving via (PersistentBlockStateMonad
               PersistentBlockStateContext
               (FocusGlobalStateM PersistentBlockStateContext g m))
    instance (MonadIO m,
              BlockStateOperations (PersistentBlockStateMonad
                                     PersistentBlockStateContext
                                     (FocusGlobalStateM PersistentBlockStateContext g m)))
             => BlockStateOperations (DB r g s m)

deriving via (PersistentBlockStateMonad
               PersistentBlockStateContext
               (FocusGlobalStateM PersistentBlockStateContext g m))
    instance (MonadIO m,
              BlockStateStorage (PersistentBlockStateMonad
                                  PersistentBlockStateContext
                                  (FocusGlobalStateM PersistentBlockStateContext g m)))
             => BlockStateStorage (DB r g s m)

-----------------------------------------------------------------------------

-- |@TreeStateM s m@ is a newtype wrapper around a monad for
-- implementing tree state monads.  The parameter @s@ should
-- be the state type of the underlying monad @m@.
--
-- For the monad @TreeStateM s m@, the underlying monad @m@ should satisfy
-- @MonadState s m@.
--
-- * If @s@ is 'SkovData bs', then the in-memory, Haskell tree state is used.
-- * If @s@ is 'SkovPersistentData ati bs', then the persistent Haskell tree state is used.
newtype TreeStateM s m a = TreeStateM {runTreeStateM :: m a}
    deriving (Functor, Applicative, Monad, MonadState s, MonadIO,
              BlockStateTypes, BlockStateQuery, BlockStateOperations, BlockStateStorage)

-- * Specializations
type MT bs m = TreeStateM (SkovData bs) m
type DT ati bs m = TreeStateM (SkovPersistentData ati bs) m

-- * Specialized implementations
-- ** Memory implementations
deriving via PureTreeStateMonad bs m
    instance ATITypes (MT bs m)

deriving via PureTreeStateMonad bs m
    instance Monad m => PerAccountDBOperations (MT bs m)

deriving via PureTreeStateMonad bs m
    instance GlobalStateTypes (MT bs m)

deriving via PureTreeStateMonad bs m
    instance (Monad m,
              Convert Transaction Transaction (PureTreeStateMonad bs m))
             => Convert Transaction Transaction (MT bs m)

deriving via PureTreeStateMonad bs m
    instance (Monad m,
              BlockPointerMonad (PureTreeStateMonad bs m))
              => BlockPointerMonad (MT bs m)

deriving via PureTreeStateMonad bs m
    instance (Monad m,
              BlockStateStorage m,
              TreeStateMonad (PureTreeStateMonad bs m))
              => TreeStateMonad (MT bs m)

-- ** Disk implementations
deriving via PersistentTreeStateMonad ati bs m
    instance ATITypes (PersistentTreeStateMonad ati bs m)
             => ATITypes (DT ati bs m)

deriving via PersistentTreeStateMonad ati bs m
    instance (Monad m, PerAccountDBOperations (PersistentTreeStateMonad ati bs m))
             => PerAccountDBOperations (DT ati bs m)

deriving via PersistentTreeStateMonad ati bs m
    instance GlobalStateTypes (DT ati bs m)

deriving via PersistentTreeStateMonad ati bs m
    instance (Monad m,
              Convert Transaction PersistentTransaction (PersistentTreeStateMonad ati bs m))
             => Convert Transaction PersistentTransaction (DT ati bs m)

deriving via PersistentTreeStateMonad ati bs m
    instance (Monad m,
              BlockPointerMonad (PersistentTreeStateMonad ati bs m))
             => BlockPointerMonad (DT ati bs m)

deriving via PersistentTreeStateMonad ati bs m
    instance (Monad m,
              BlockStateStorage m,
              TreeStateMonad (PersistentTreeStateMonad ati bs m))
             => TreeStateMonad (DT ati bs m)

-- |A newtype wrapper for providing instances of global state monad classes.
-- The block state monad instances are derived directly from 'BlockStateM'.
-- The tree state monad instances are derived directly from 'TreeStateM'.
-- The arguments c, r, g, s, m, a are as in BlockStateM, whereas the argument @db@
-- is an additional context that manages auxiliary databases not needed by consensus.
-- In particular this means the index of transactions that affect a given account.
newtype GlobalStateM db c r g s m a = GlobalStateM {runGlobalStateM :: m a}
    deriving (Functor, Applicative, Monad, MonadReader r, MonadState s, MonadIO)
    deriving (BlockStateTypes) via (BlockStateM c r g s m)

-- * Specializations

type TB g c r s m = TreeStateM g (BlockStateM c r g s m)

-- * Generic implementations

deriving via BlockStateM c r g s m
    instance (Monad m, BlockStateQuery (BlockStateM c r g s m))
             => BlockStateQuery (GlobalStateM db c r g s m)

deriving via BlockStateM c r g s m
    instance (BlockStateQuery (GlobalStateM db c r g s m),
              BlockStateOperations (BlockStateM c r g s m))
             => BlockStateOperations (GlobalStateM db c r g s m)

deriving via BlockStateM c r g s m
    instance (BlockStateOperations (GlobalStateM db c r g s m),
              BlockStateStorage (BlockStateM c r g s m))
             => BlockStateStorage (GlobalStateM db c r g s m)

deriving via TB g c r s m
    instance ATITypes (TB g c r s m)
             => ATITypes (GlobalStateM db c r g s m)

deriving via TB g c r s m
    instance (Monad m, PerAccountDBOperations (TB g c r s m))
             => PerAccountDBOperations (GlobalStateM db c r g s m)

deriving via TB g c r s m
    instance GlobalStateTypes (TB g c r s m)
             => GlobalStateTypes (GlobalStateM db c r g s m)

deriving via TB g c r s m
    instance (Monad m,
              Convert Transaction t (TB g c r s m))
             => Convert Transaction t (GlobalStateM db c r g s m)

deriving via TB g c r s m
    instance (Monad m,
              BlockPointerMonad (TB g c r s m))
             => BlockPointerMonad (GlobalStateM db c r g s m)

deriving via TB g c r s m
    instance (Monad m,
              BlockStateStorage (BlockStateM c r g s m),
              TreeStateMonad (TB g c r s m))
             => TreeStateMonad (GlobalStateM db c r g s m)

-----------------------------------------------------------------------------

-- |Configuration that uses in-memory, Haskell implementations for both tree state and block state.
data MemoryTreeMemoryBlockConfig = MTMBConfig RuntimeParameters GenesisData BS.BlockState

-- |Configuration that uses the in-memory, Haskell implementation of tree state and the
-- persistent Haskell implementation of block state.
data MemoryTreeDiskBlockConfig = MTDBConfig RuntimeParameters GenesisData BS.BlockState

-- |Configuration that uses the disk implementation for both the tree state
-- and the block state
data DiskTreeDiskBlockConfig = DTDBConfig RuntimeParameters GenesisData BS.BlockState

-- |Configuration that uses the disk implementation for both the tree state
-- and the block state, as well as a
data DiskTreeDiskBlockWithLogConfig = DTDBWLConfig {
  configRP :: RuntimeParameters,
  configGD :: GenesisData,
  configBS :: BS.BlockState,
  configTxLog :: !ByteString
  }

type family GSContext c where
  GSContext MemoryTreeMemoryBlockConfig = ()
  GSContext MemoryTreeDiskBlockConfig = PersistentBlockStateContext
  GSContext DiskTreeDiskBlockConfig = PersistentBlockStateContext
  GSContext DiskTreeDiskBlockWithLogConfig = PersistentBlockStateContext

type family GSState c where
  GSState MemoryTreeMemoryBlockConfig = SkovData BS.BlockState
  GSState MemoryTreeDiskBlockConfig = SkovData PersistentBlockState
  GSState DiskTreeDiskBlockConfig = SkovPersistentData () PersistentBlockState
  GSState DiskTreeDiskBlockWithLogConfig = SkovPersistentData DiskDump PersistentBlockState

type family GSLogContext c where
  GSLogContext MemoryTreeMemoryBlockConfig = NoLogContext
  GSLogContext MemoryTreeDiskBlockConfig = NoLogContext
  GSLogContext DiskTreeDiskBlockConfig = NoLogContext
  GSLogContext DiskTreeDiskBlockWithLogConfig = PerAccountAffectIndex

-- |This class is implemented by types that determine configurations for the global state.
class GlobalStateConfig c where
    -- |Generate context and state from the initial configuration. This may
    -- have 'IO' side effects to set up any necessary storage.
    initialiseGlobalState :: c -> IO (GSContext c, GSState c, GSLogContext c)
    -- |Shutdown the global state.
    shutdownGlobalState :: Proxy c -> GSContext c -> GSState c -> GSLogContext c -> IO ()

instance GlobalStateConfig MemoryTreeMemoryBlockConfig where
    initialiseGlobalState (MTMBConfig rtparams gendata bs) = do
      return ((), initialSkovData rtparams gendata bs, NoLogContext)
    shutdownGlobalState _ _ _ _ = return ()

-- |Configuration that uses the Haskell implementation of tree state and the
-- in-memory, Haskell implmentation of the block state.
instance GlobalStateConfig MemoryTreeDiskBlockConfig where
    initialiseGlobalState (MTDBConfig rtparams gendata bs) = do
        pbscBlobStore <- createTempBlobStore . (<.> "dat") . rpBlockStateFile $ rtparams
        pbscModuleCache <- newIORef emptyModuleCache
        let pbsc = PersistentBlockStateContext{..}
        pbs <- makePersistent bs
        _ <- runPut <$> runReaderT (runPersistentBlockStateMonad (putBlockState pbs)) pbsc
        return (pbsc, initialSkovData rtparams gendata pbs, NoLogContext)
    shutdownGlobalState _ (PersistentBlockStateContext{..}) _ _ = do
        destroyTempBlobStore pbscBlobStore
        writeIORef pbscModuleCache Persistent.emptyModuleCache

instance GlobalStateConfig DiskTreeDiskBlockConfig where
    initialiseGlobalState (DTDBConfig rtparams gendata bs) = do
        pbscBlobStore <- createTempBlobStore . (<.> "dat") . rpBlockStateFile $ rtparams
        pbscModuleCache <- newIORef emptyModuleCache
        pbs <- makePersistent bs
        let pbsc = PersistentBlockStateContext{..}
        serBS <- runReaderT (runPersistentBlockStateMonad (putBlockState pbs)) pbsc
        isd <- initialSkovPersistentData rtparams gendata pbs ((), NoLogContext) serBS
        return (pbsc, isd, NoLogContext)
    shutdownGlobalState _ (PersistentBlockStateContext{..}) _ _ = do
        destroyTempBlobStore pbscBlobStore
        writeIORef pbscModuleCache Persistent.emptyModuleCache

instance GlobalStateConfig DiskTreeDiskBlockWithLogConfig where
    initialiseGlobalState (DTDBWLConfig rtparams gendata bs txLog) = do
        pbscBlobStore <- createTempBlobStore . (<.> "dat") . rpBlockStateFile $ rtparams
        pbscModuleCache <- newIORef emptyModuleCache
        pbs <- makePersistent bs
        let pbsc = PersistentBlockStateContext{..}
        serBS <- runReaderT (runPersistentBlockStateMonad (putBlockState pbs)) pbsc
        handle <- connectPostgres txLog
        createTable handle
        let ati = defaultValue
        isd <- initialSkovPersistentData rtparams gendata pbs (ati, PAAIConfig handle) serBS
        return (pbsc, isd, PAAIConfig handle)
    shutdownGlobalState _ (PersistentBlockStateContext{..}) _ (PAAIConfig handle) = do
        destroyTempBlobStore pbscBlobStore
        writeIORef pbscModuleCache Persistent.emptyModuleCache
        destroyAllResources handle
