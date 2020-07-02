{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Concordium.GlobalState.Persistent.BlockState where

import Data.Serialize
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Control.Monad.Reader.Class
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans
import Control.Monad
import Lens.Micro.Platform
import Concordium.Utils
import qualified Data.Set as Set
import Data.Maybe
import qualified Data.Vector as Vec

import GHC.Generics (Generic)

import qualified Concordium.Crypto.SHA256 as H
import qualified Concordium.Types.Acorn.Core as Core
import Concordium.Types.Acorn.Interfaces
import Concordium.Types
import Concordium.Types.Execution
import qualified Concordium.ID.Types as ID
import Acorn.Types (linkExprWithMaxSize)

import Concordium.GlobalState.BakerInfo
import qualified Concordium.GlobalState.Basic.BlockState.Bakers as BB
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.Trie as Trie
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Types
import qualified Concordium.GlobalState.IdentityProviders as IPS
import qualified Concordium.GlobalState.Rewards as Rewards
import qualified Concordium.GlobalState.Persistent.Account as Account
import Concordium.GlobalState.Persistent.Bakers
import qualified Concordium.GlobalState.Persistent.Instances as Instances
import qualified Concordium.Types.Transactions as Transactions
import qualified Concordium.Types.Execution as Transactions
import Concordium.GlobalState.Persistent.Instances(PersistentInstance(..), PersistentInstanceParameters(..), CacheableInstanceParameters(..))
import Concordium.GlobalState.Instance (Instance(..),InstanceParameters(..),makeInstanceHash')
import qualified Concordium.GlobalState.Basic.BlockState as Basic
import qualified Concordium.GlobalState.Modules as TransientMods
import Concordium.GlobalState.SeedState
import Concordium.Logger (MonadLogger)

type PersistentBlockState = IORef (BufferedRef BlockStatePointers)

data BlockStatePointers = BlockStatePointers {
    bspAccounts :: !Account.Accounts,
    bspInstances ::  !Instances.Instances,
    bspModules :: BufferedRef Modules,
    bspBank :: !Rewards.BankStatus,
    bspIdentityProviders :: !(BufferedRef IPS.IdentityProviders),
    bspBirkParameters :: !PersistentBirkParameters,
    bspCryptographicParameters :: !(BufferedRef CryptographicParameters),
    -- FIXME: Store transaction outcomes in a way that allows for individual indexing.
    bspTransactionOutcomes :: !Transactions.TransactionOutcomes
}

instance (MonadBlobStore m BlobRef, MonadIO m) => BlobStorable m BlobRef BlockStatePointers where
    storeUpdate p bsp0@BlockStatePointers{..} = do
        (paccts, bspAccounts') <- storeUpdate p bspAccounts
        (pinsts, bspInstances') <- storeUpdate p bspInstances
        (pmods, bspModules') <- storeUpdate p bspModules
        (pips, bspIdentityProviders') <- storeUpdate p bspIdentityProviders
        (pbps, bspBirkParameters') <- storeUpdate p bspBirkParameters
        (pcryptps, bspCryptographicParameters') <- storeUpdate p bspCryptographicParameters
        let putBSP = do
                paccts
                pinsts
                pmods
                put bspBank
                pips
                pbps
                pcryptps
                put bspTransactionOutcomes
        return (putBSP, bsp0 {
                    bspAccounts = bspAccounts',
                    bspInstances = bspInstances',
                    bspModules = bspModules',
                    bspIdentityProviders = bspIdentityProviders',
                    bspBirkParameters = bspBirkParameters',
                    bspCryptographicParameters = bspCryptographicParameters'
                })
    store p bsp = fst <$> storeUpdate p bsp
    load p = do
        maccts <- label "Accounts" $ load p
        minsts <- label "Instances" $ load p
        mmods <- label "Modules" $ load p
        bspBank <- label "Bank" $ get
        mpips <- label "Identity providers" $ load p
        mbps <- label "Birk parameters" $ load p
        mcryptps <- label "Cryptographic parameters" $ load p
        bspTransactionOutcomes <- label "Transaction outcomes" $ get
        return $! do
            bspAccounts <- maccts
            bspInstances <- minsts
            bspModules <- mmods
            bspIdentityProviders <- mpips
            bspBirkParameters <- mbps
            bspCryptographicParameters <- mcryptps
            return $! BlockStatePointers{..}

data PersistentModule = PersistentModule {
    pmInterface :: !(Interface Core.UA),
    pmValueInterface :: !(UnlinkedValueInterface Core.NoAnnot),
    pmIndex :: !ModuleIndex,
    pmSource :: !(Core.Module Core.UA)
}

persistentModuleToModule :: PersistentModule -> Module
persistentModuleToModule PersistentModule{..} = Module {
    moduleInterface = pmInterface,
    moduleValueInterface = pmValueInterface,
    moduleIndex = pmIndex,
    moduleSource = pmSource
}


instance Serialize PersistentModule where
    put PersistentModule{..} = put pmInterface >> put pmValueInterface >> put pmIndex >> put pmSource
    get = PersistentModule <$> get <*> get <*> get <*> get

instance (MonadBlobStore m ref) => BlobStorable m ref PersistentModule

data Modules = Modules {
    modules :: Trie.TrieN (BufferedBlobbed BlobRef) Core.ModuleRef PersistentModule,
    nextModuleIndex :: !ModuleIndex,
    runningHash :: !H.Hash
}

emptyModules :: Modules
emptyModules = Modules {
        modules = Trie.empty,
        nextModuleIndex = 0,
        runningHash = H.hash ""
    }

instance (MonadBlobStore m BlobRef, MonadIO m) => BlobStorable m BlobRef Modules where
    storeUpdate p ms@Modules{..} = do
        (pm, modules') <- storeUpdate p modules
        return (pm >> put nextModuleIndex >> put runningHash, ms {modules = modules'})
    store p m = fst <$> storeUpdate p m
    load p = do
        mmodules <- load p
        nextModuleIndex <- get
        runningHash <- get
        return $ do
            modules <- mmodules
            return Modules{..}

makePersistentModules :: MonadIO m => TransientMods.Modules -> m Modules
makePersistentModules (TransientMods.Modules m nmi rh) = do
    m' <- Trie.fromList $ upd <$> HM.toList m
    return $ Modules m' nmi rh
    where
        upd (mref, TransientMods.MemModule{..}) = (mref, PersistentModule{
                pmInterface = mmoduleInterface,
                pmValueInterface = mmoduleValueInterface,
                pmIndex = mmoduleIndex,
                pmSource = mmoduleSource
            })

data PersistentBirkParameters = PersistentBirkParameters {
    _birkElectionDifficulty :: ElectionDifficulty,
    -- |The current stake of bakers. All updates should be to this state.
    _birkCurrentBakers :: !PersistentBakers,
    -- |The state of bakers at the end of the previous epoch,
    -- will be used as lottery bakers in next epoch.
    _birkPrevEpochBakers :: !(BufferedRef PersistentBakers),
    -- |The state of the bakers fixed before previous epoch,
    -- the lottery power and reward account is used in leader election.
    _birkLotteryBakers :: !(BufferedRef PersistentBakers),
    _birkSeedState :: !SeedState
} deriving (Generic, Show)

makeLenses ''PersistentBirkParameters

instance (MonadBlobStore m BlobRef, MonadIO m) => BlobStorable m BlobRef PersistentBirkParameters where
    storeUpdate p bps@PersistentBirkParameters{..} = do
        (ppebs, prevEpochBakers) <- storeUpdate p _birkPrevEpochBakers
        (plbs, lotteryBakers) <- storeUpdate p _birkLotteryBakers
        (pcbs, currBakers) <- storeUpdate p _birkCurrentBakers
        let putBSP = do
                put _birkElectionDifficulty
                pcbs
                ppebs
                plbs
                put _birkSeedState
        return (putBSP, bps {
                    _birkCurrentBakers = currBakers,
                    _birkPrevEpochBakers = prevEpochBakers,
                    _birkLotteryBakers = lotteryBakers
                })
    store p bps = fst <$> storeUpdate p bps
    load p = do
        _birkElectionDifficulty <- label "Election difficulty" get
        mcbs <- label "Current bakers" $ load p
        mpebs <- label "Previous-epoch bakers" $ load p
        mlbs <- label "Lottery bakers" $ load p
        _birkSeedState <- label "Seed state" get
        return $! do
            _birkCurrentBakers <- mcbs
            _birkPrevEpochBakers <- mpebs
            _birkLotteryBakers <- mlbs
            return $! PersistentBirkParameters{..}

data ModuleCache = ModuleCache {
    _cachedLinkedDefs :: HM.HashMap (Core.ModuleRef, Core.Name) (LinkedExprWithDeps Core.NoAnnot),
    _cachedLinkedContracts :: HM.HashMap (Core.ModuleRef, Core.TyName) (LinkedContractValue Core.NoAnnot)
}
makeLenses ''ModuleCache

emptyModuleCache :: ModuleCache
emptyModuleCache = ModuleCache HM.empty HM.empty

class HasModuleCache a where
    moduleCache :: a -> IORef ModuleCache

makePersistentBirkParameters :: MonadIO m => Basic.BasicBirkParameters -> m PersistentBirkParameters
makePersistentBirkParameters Basic.BasicBirkParameters{..} = do
    prevEpochBakers <- makeBufferedRef =<< makePersistentBakers _birkPrevEpochBakers
    lotteryBakers <- makeBufferedRef =<< makePersistentBakers _birkLotteryBakers
    currBakers <- makePersistentBakers _birkCurrentBakers
    return $ PersistentBirkParameters
        _birkElectionDifficulty
        currBakers
        prevEpochBakers
        lotteryBakers
        _birkSeedState

makePersistent :: MonadIO m => Basic.BlockState -> m PersistentBlockState
makePersistent Basic.BlockState{..} = do
    persistentBlockInstances <- Instances.makePersistent _blockInstances
    persistentBirkParameters <- makePersistentBirkParameters _blockBirkParameters
    liftIO $ do
        persistentMods <- makePersistentModules _blockModules
        modules <- makeBufferedRef $! persistentMods
        identityProviders <- makeBufferedRef $! _blockIdentityProviders
        cryptographicParameters <- makeBufferedRef $! _blockCryptographicParameters
        blockAccounts <- Account.makePersistent _blockAccounts
        bsp <- makeBufferedRef $ BlockStatePointers {
            bspAccounts = blockAccounts
            , bspInstances = persistentBlockInstances
            , bspModules = modules
            , bspBank = _blockBank
            , bspIdentityProviders = identityProviders
            , bspBirkParameters = persistentBirkParameters
            , bspCryptographicParameters = cryptographicParameters
            , bspTransactionOutcomes = _blockTransactionOutcomes
            }
        newIORef $! bsp

initialPersistentState :: MonadIO m => Basic.BasicBirkParameters
             -> CryptographicParameters
             -> [Account]
             -> [IPS.IpInfo]
             -> Amount
             -> m PersistentBlockState
initialPersistentState bps cps accts ips amt = makePersistent $ Basic.initialState bps cps accts ips amt


newtype LinkerWrapper r m a = LinkerWrapper { runLinkerWrapper :: ReaderT PersistentBlockState m a }
    deriving (Functor, Applicative, Monad, MonadReader PersistentBlockState, MonadTrans)

instance (MonadBlobStore m BlobRef, MonadIO m, MonadReader r m, HasModuleCache r) => LinkerMonad Core.NoAnnot (LinkerWrapper r m) where
    getExprInModule modRef name = do
        blockState <- ask
        mmod <- lift $ doGetModule blockState modRef
        case mmod of
            Nothing -> return Nothing
            Just Module{..} -> return (HM.lookup name (viDefs moduleValueInterface))
    tryGetLinkedExpr modref name = do
        blockState <- ask
        lift $ doTryGetLinkedExpr blockState modref name
    putLinkedExpr modref name linked = do
        blockState <- ask
        void $ lift $ doPutLinkedExpr blockState modref name linked

-- |Mostly empty block state, apart from using 'Rewards.genesisBankStatus' which
-- has hard-coded initial values for amount of gtu in existence.
emptyBlockState :: MonadIO m => PersistentBirkParameters -> CryptographicParameters -> m PersistentBlockState
emptyBlockState bspBirkParameters cryptParams = liftIO $ do
    modules <- makeBufferedRef $! emptyModules
    identityProviders <- makeBufferedRef $! IPS.emptyIdentityProviders
    cryptographicParameters <- makeBufferedRef $! cryptParams
    bsp <- makeBufferedRef $ BlockStatePointers {
        bspAccounts = Account.emptyAccounts
        , bspInstances = Instances.emptyInstances
        , bspModules = modules
        , bspBank = Rewards.emptyBankStatus
        , bspIdentityProviders = identityProviders
        , bspCryptographicParameters = cryptographicParameters
        , bspTransactionOutcomes = Transactions.emptyTransactionOutcomes
        ,..
    }
    newIORef $! bsp




doLinkContract :: (MonadBlobStore m BlobRef, MonadIO m, MonadReader r m, HasModuleCache r) =>
    PersistentBlockState -> Core.ModuleRef -> Module -> Core.TyName -> m (LinkedContractValue Core.NoAnnot)
doLinkContract pbs mref m cname = do
    mcontract <- doTryGetLinkedContract pbs mref cname
    case mcontract of
        Just contract -> return contract
        Nothing -> do
            let unlinked = viContracts (moduleValueInterface m) HM.! cname
            linked <- flip runReaderT pbs $ runLinkerWrapper $ do
                cvInitMethod <- myLink (cvInitMethod unlinked)
                cvReceiveMethod <- myLink (cvReceiveMethod unlinked)
                cvImplements <- mapM (\iv -> do
                                        ivSenders <- mapM myLink (ivSenders iv)
                                        ivGetters <- mapM myLink (ivGetters iv)
                                        return ImplementsValue{..}
                                    ) (cvImplements unlinked)
                return ContractValue{..}
            _ <- doPutLinkedContract pbs mref cname linked
            return linked
    where
        myLink ule = fromJust <$> linkExprWithMaxSize mref ule maxBound

fromPersistentInstance :: (MonadBlobStore m BlobRef, MonadIO m, MonadReader r m, HasModuleCache r) =>
    PersistentBlockState -> Instances.PersistentInstance -> m Instance
fromPersistentInstance _ Instances.PersistentInstance{pinstanceCachedParameters = (Some CacheableInstanceParameters{..}), ..} = do
    PersistentInstanceParameters{..} <- loadBufferedRef pinstanceParameters
    let instanceParameters = InstanceParameters {
            instanceAddress = pinstanceAddress,
            instanceOwner = pinstanceOwner,
            instanceContractModule = pinstanceContractModule,
            instanceContract = pinstanceContract,
            instanceReceiveFun = pinstanceReceiveFun,
            instanceModuleInterface = pinstanceModuleInterface,
            instanceModuleValueInterface = pinstanceModuleValueInterface,
            instanceMessageType = pinstanceMessageType,
            instanceImplements = pinstanceImplements,
            instanceParameterHash = pinstanceParameterHash
        }
    return Instance{
            instanceModel = pinstanceModel,
            instanceAmount = pinstanceAmount,
            instanceHash = pinstanceHash,
            ..
        }
fromPersistentInstance pbs Instances.PersistentInstance{..} = do
    PersistentInstanceParameters{..} <- loadBufferedRef pinstanceParameters
    doGetModule pbs pinstanceContractModule >>= \case
        Nothing -> error "fromPersistentInstance: unresolvable module" -- TODO: Possibly don't error here
        Just m -> do
            conVal <- doLinkContract pbs pinstanceContractModule m pinstanceContract
            let instanceParameters = InstanceParameters {
                    instanceAddress = pinstanceAddress,
                    instanceOwner = pinstanceOwner,
                    instanceContractModule = pinstanceContractModule,
                    instanceContract = pinstanceContract,
                    instanceReceiveFun = fst (cvReceiveMethod conVal),
                    instanceModuleInterface = moduleInterface m,
                    instanceModuleValueInterface = moduleValueInterface m,
                    instanceMessageType = msgTy (exportedContracts (moduleInterface m) HM.! pinstanceContract),
                    instanceImplements = cvImplements conVal,
                    instanceParameterHash = pinstanceParameterHash
                }
            return Instance{
                    instanceModel = pinstanceModel,
                    instanceAmount = pinstanceAmount,
                    instanceHash = pinstanceHash,
                    ..
                }

loadPBS :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> m BlockStatePointers
loadPBS = loadBufferedRef <=< liftIO . readIORef

storePBS :: (MonadIO m) => PersistentBlockState -> BlockStatePointers -> m PersistentBlockState
storePBS pbs bsp = liftIO $ do
    pbsp <- makeBufferedRef bsp
    writeIORef pbs pbsp
    return pbs

doGetModule :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> Core.ModuleRef -> m (Maybe Module)
doGetModule s modRef = do
        bsp <- loadPBS s
        mods <- loadBufferedRef (bspModules bsp)
        fmap persistentModuleToModule <$> Trie.lookup modRef (modules mods)

doGetModuleList :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> m [Core.ModuleRef]
doGetModuleList s = do
        bsp <- loadPBS s
        mods <- loadBufferedRef (bspModules bsp)
        Trie.keys (modules mods)

doPutNewModule :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState
    -> Core.ModuleRef
    -> Interface Core.UA
    -> UnlinkedValueInterface Core.NoAnnot
    -> Core.Module Core.UA
    -> m (Bool, PersistentBlockState)
doPutNewModule pbs mref pmInterface pmValueInterface pmSource = do
        bsp <- loadPBS pbs
        mods <- loadBufferedRef (bspModules bsp)
        let
            newMod = PersistentModule{pmIndex = nextModuleIndex mods, ..}
            tryIns Nothing = return (True, Trie.Insert newMod)
            tryIns (Just _) = return (False, Trie.NoChange)
        (b, modules') <- Trie.adjust tryIns mref (modules mods)
        if b then do
            let
                newMods = mods {modules = modules', nextModuleIndex = nextModuleIndex mods + 1}
            modules <- makeBufferedRef newMods
            (True,) <$> storePBS pbs (bsp {bspModules = modules})
        else
            return (False, pbs)

doTryGetLinkedExpr :: (MonadIO m, MonadReader r m, HasModuleCache r) => PersistentBlockState -> Core.ModuleRef -> Core.Name -> m (Maybe (LinkedExprWithDeps Core.NoAnnot))
doTryGetLinkedExpr _ modRef n = do
        cache <- asks moduleCache >>= liftIO . readIORef
        return $! HM.lookup (modRef, n) (_cachedLinkedDefs cache)

doPutLinkedExpr :: (MonadIO m, MonadReader r m, HasModuleCache r) => PersistentBlockState -> Core.ModuleRef -> Core.Name -> LinkedExprWithDeps Core.NoAnnot -> m PersistentBlockState
doPutLinkedExpr pbs modRef n !le = do
        cacheRef <- asks moduleCache
        liftIO $ modifyIORef' cacheRef (cachedLinkedDefs %~ HM.insert (modRef, n) le)
        return pbs

doTryGetLinkedContract :: (MonadIO m, MonadReader r m, HasModuleCache r) => PersistentBlockState -> Core.ModuleRef -> Core.TyName -> m (Maybe (LinkedContractValue Core.NoAnnot))
doTryGetLinkedContract _ modRef n = do
        cache <- asks moduleCache >>= liftIO . readIORef
        return $! HM.lookup (modRef, n) (_cachedLinkedContracts cache)

doPutLinkedContract :: (MonadIO m, MonadReader r m, HasModuleCache r) => PersistentBlockState -> Core.ModuleRef -> Core.TyName -> LinkedContractValue Core.NoAnnot -> m PersistentBlockState
doPutLinkedContract pbs modRef n !lc = do
        cacheRef <- asks moduleCache
        liftIO $ modifyIORef' cacheRef (cachedLinkedContracts %~ HM.insert (modRef, n) lc)
        return pbs

doGetBlockBirkParameters :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> m PersistentBirkParameters
doGetBlockBirkParameters pbs = bspBirkParameters <$> loadPBS pbs

doAddBaker :: (MonadIO m, MonadBlobStore (PersistentBlockStateMonad r m) BlobRef) => PersistentBlockState -> BakerInfo -> PersistentBlockStateMonad r m (Either BakerError BakerId, PersistentBlockState)
doAddBaker pbs binfo = do
        bsp <- loadPBS pbs
        createBaker binfo (bspBirkParameters bsp ^. birkCurrentBakers) >>= \case
            Left err -> return (Left err, pbs)
            Right (bid, newBakers) -> (Right bid,) <$> storePBS pbs (bsp {bspBirkParameters = bspBirkParameters bsp & birkCurrentBakers .~ newBakers})

doUpdateBaker :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> BB.BakerUpdate -> m (Bool, PersistentBlockState)
doUpdateBaker pbs bupdate = do
        bsp <- loadPBS pbs
        updateBaker bupdate (bspBirkParameters bsp ^. birkCurrentBakers) >>= \case
            Nothing -> return (False, pbs)
            Just newBakers ->
              (True, ) <$!> storePBS pbs (bsp {bspBirkParameters =  bspBirkParameters bsp & birkCurrentBakers .~ newBakers})

doRemoveBaker :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> BakerId -> m (Bool, PersistentBlockState)
doRemoveBaker pbs bid = do
        bsp <- loadPBS pbs
        (rv, newBakers) <- removeBaker bid (bspBirkParameters bsp ^. birkCurrentBakers)
        (rv,) <$> storePBS pbs (bsp {bspBirkParameters = bspBirkParameters bsp & birkCurrentBakers .~ newBakers})

doGetRewardStatus :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> m Rewards.BankStatus
doGetRewardStatus pbs = bspBank <$> loadPBS pbs

doSetInflation :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> Amount -> m PersistentBlockState
doSetInflation pbs amount = do
        bsp <- loadPBS pbs
        storePBS pbs (bsp {bspBank = bspBank bsp & Rewards.mintedGTUPerSlot .~ amount})

doMint :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> Amount -> m (Amount, PersistentBlockState)
doMint pbs amount = do
        bsp <- loadPBS pbs
        let newBank = bspBank bsp & (Rewards.totalGTU +~ amount) . (Rewards.centralBankGTU +~ amount)
        (newBank ^. Rewards.centralBankGTU,) <$> storePBS pbs (bsp {bspBank = newBank})

doDecrementCentralBankGTU :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> Amount -> m (Amount, PersistentBlockState)
doDecrementCentralBankGTU pbs amount = do
        bsp <- loadPBS pbs
        let newBank = bspBank bsp & Rewards.centralBankGTU -~ amount
        (newBank ^. Rewards.centralBankGTU,) <$> storePBS pbs (bsp {bspBank = newBank})

doGetAccount :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> AccountAddress -> m (Maybe Account)
doGetAccount pbs addr = do
        bsp <- loadPBS pbs
        Account.getAccount addr (bspAccounts bsp)

doAccountList :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> m [AccountAddress]
doAccountList pbs = do
        bsp <- loadPBS pbs
        Account.accountAddresses (bspAccounts bsp)

doRegIdExists :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> ID.CredentialRegistrationID -> m Bool
doRegIdExists pbs regid = do
        bsp <- loadPBS pbs
        fst <$> Account.regIdExists regid (bspAccounts bsp)

doPutNewAccount :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> Account -> m (Bool, PersistentBlockState)
doPutNewAccount pbs acct = do
        bsp <- loadPBS pbs
        (res, accts') <- Account.putNewAccount acct (bspAccounts bsp)
        (res,) <$> storePBS pbs (bsp {bspAccounts = accts'})

doModifyAccount :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> AccountUpdate -> m PersistentBlockState
doModifyAccount pbs aUpd@AccountUpdate{..} = do
        bsp <- loadPBS pbs
        -- Do the update to the account
        (mbalinfo, accts1) <- Account.updateAccount upd _auAddress (bspAccounts bsp)
        -- If we deploy a credential, record it
        accts2 <- case _auCredential of
            Just cdi -> Account.recordRegId (ID.cdvRegId cdi) accts1
            Nothing -> return accts1
        -- If the amount is changed update the delegate stake
        birkParams1 <- case (_auAmount, mbalinfo) of
                (Just deltaAmnt, Just delegate) -> do
                    newCurrBakers <- modifyStake delegate deltaAmnt (bspBirkParameters bsp ^. birkCurrentBakers)
                    return $ bspBirkParameters bsp & birkCurrentBakers .~ newCurrBakers
                _ -> return $ bspBirkParameters bsp
        storePBS pbs (bsp {bspAccounts = accts2, bspBirkParameters = birkParams1})
    where
        upd oldAccount = return ((oldAccount ^. accountStakeDelegate), updateAccount aUpd oldAccount)

doGetInstance :: (MonadBlobStore m BlobRef, MonadIO m, MonadReader r m, HasModuleCache r) => PersistentBlockState -> ContractAddress -> m (Maybe Instance)
doGetInstance pbs caddr = do
        bsp <- loadPBS pbs
        minst <- Instances.lookupContractInstance caddr (bspInstances bsp)
        forM minst $ fromPersistentInstance pbs

doContractInstanceList :: (MonadBlobStore m BlobRef, MonadIO m, MonadReader r m, HasModuleCache r) => PersistentBlockState -> m [Instance]
doContractInstanceList pbs = do
        bsp <- loadPBS pbs
        insts <- Instances.allInstances (bspInstances bsp)
        mapM (fromPersistentInstance pbs) insts

doPutNewInstance :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> (ContractAddress -> Instance) -> m (ContractAddress, PersistentBlockState)
doPutNewInstance pbs fnew = do
        bsp <- loadPBS pbs
        -- Create the instance
        (inst, insts) <- Instances.newContractInstance fnew' (bspInstances bsp)
        let ca = instanceAddress (instanceParameters inst)
        -- Update the owner account's set of instances
        let updAcct oldAccount = return (oldAccount ^. accountStakeDelegate, oldAccount & accountInstances %~ Set.insert ca)
        (mdelegate, accts) <- Account.updateAccount updAcct (instanceOwner (instanceParameters inst)) (bspAccounts bsp)
        -- Update the stake delegate
        case mdelegate of
            Nothing -> error "Invalid contract owner"
            Just delegate -> do
                newCurrBakers <- modifyStake delegate (amountToDelta (instanceAmount inst)) (bspBirkParameters bsp ^. birkCurrentBakers)
                (ca,) <$> storePBS pbs bsp{
                                    bspInstances = insts,
                                    bspAccounts = accts,
                                    bspBirkParameters = bspBirkParameters bsp & birkCurrentBakers .~ newCurrBakers
                                }
    where
        fnew' ca = let inst@Instance{instanceParameters = InstanceParameters{..}, ..} = fnew ca in do
            params <- makeBufferedRef $ PersistentInstanceParameters {
                                            pinstanceAddress = instanceAddress,
                                            pinstanceOwner = instanceOwner,
                                            pinstanceContractModule = instanceContractModule,
                                            pinstanceContract = instanceContract,
                                            pinstanceParameterHash = instanceParameterHash
                                        }
            return (inst, PersistentInstance{
                pinstanceParameters = params,
                pinstanceCachedParameters = Some (CacheableInstanceParameters{
                        pinstanceReceiveFun = instanceReceiveFun,
                        pinstanceModuleInterface = instanceModuleInterface,
                        pinstanceModuleValueInterface = instanceModuleValueInterface,
                        pinstanceMessageType = instanceMessageType,
                        pinstanceImplements = instanceImplements
                    }),
                pinstanceModel = instanceModel,
                pinstanceAmount = instanceAmount,
                pinstanceHash = instanceHash
            })

doModifyInstance :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> ContractAddress -> AmountDelta -> Value Core.NoAnnot -> m PersistentBlockState
doModifyInstance pbs caddr deltaAmnt val = do
        bsp <- loadPBS pbs
        -- Update the instance
        Instances.updateContractInstance upd caddr (bspInstances bsp) >>= \case
            Nothing -> error "Invalid contract address"
            Just (Nothing, insts) -> -- no change to staking
                storePBS pbs bsp{bspInstances = insts}
            Just (Just owner, insts) ->
                -- Lookup the owner account and update its stake delegate
                Account.getAccount owner (bspAccounts bsp) >>= \case
                    Nothing -> error "Invalid contract owner"
                    Just acct -> do
                        newCurrBakers <- modifyStake (_accountStakeDelegate acct) deltaAmnt (bspBirkParameters bsp ^. birkCurrentBakers)
                        storePBS pbs bsp{
                            bspInstances = insts,
                            bspBirkParameters = bspBirkParameters bsp & birkCurrentBakers .~ newCurrBakers
                        }
    where
        upd oldInst = do
            (piParams, newParamsRef) <- cacheBufferedRef (pinstanceParameters oldInst)
            if deltaAmnt == 0 then
                return (Nothing, rehash (pinstanceParameterHash piParams) $ oldInst {pinstanceParameters = newParamsRef, pinstanceModel = val})
            else do
                acct <- pinstanceOwner <$> loadBufferedRef (pinstanceParameters oldInst)
                return (Just acct, rehash (pinstanceParameterHash piParams) $ oldInst {pinstanceParameters = newParamsRef, pinstanceAmount = applyAmountDelta deltaAmnt (pinstanceAmount oldInst), pinstanceModel = val})
        rehash iph inst@(PersistentInstance {..}) = inst {pinstanceHash = makeInstanceHash' iph pinstanceModel pinstanceAmount}

doDelegateStake :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> AccountAddress -> Maybe BakerId -> m (Bool, PersistentBlockState)
doDelegateStake pbs aaddr target = do
        bsp <- loadPBS pbs
        targetValid <- case target of
                Nothing -> return True
                Just bid -> do
                    bInfo <- Trie.lookup bid $ bspBirkParameters bsp ^. birkCurrentBakers . bakerMap
                    return $ isJust bInfo
        if targetValid then do
            let updAcc acct = return ((acct ^. accountStakeDelegate, acct ^. accountAmount, Set.toList $ acct ^. accountInstances),
                                acct & accountStakeDelegate .~ target)
            Account.updateAccount updAcc aaddr (bspAccounts bsp) >>= \case
                (Nothing, _) -> error "Invalid account address"
                (Just (acctOldTarget, acctBal, acctInsts), accts) -> do
                    instBals <- forM acctInsts $ \caddr -> maybe (error "Invalid contract instance") pinstanceAmount <$> Instances.lookupContractInstance caddr (bspInstances bsp)
                    let stake = acctBal + sum instBals
                    newCurrBakers <- removeStake acctOldTarget stake =<< addStake target stake (bspBirkParameters bsp ^. birkCurrentBakers)
                    pbs' <- storePBS pbs bsp{
                            bspAccounts = accts,
                            bspBirkParameters = bspBirkParameters bsp & birkCurrentBakers .~ newCurrBakers
                        }
                    return (True, pbs')
        else return (False, pbs)

doGetIdentityProvider :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> ID.IdentityProviderIdentity -> m (Maybe IPS.IpInfo)
doGetIdentityProvider pbs ipId = do
        bsp <- loadPBS pbs
        ips <- loadBufferedRef (bspIdentityProviders bsp)
        return $! IPS.idProviders ips ^? ix ipId

doGetCryptoParams :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> m CryptographicParameters
doGetCryptoParams pbs = do
        bsp <- loadPBS pbs
        loadBufferedRef (bspCryptographicParameters bsp)

doGetTransactionOutcome :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> Transactions.TransactionIndex -> m (Maybe TransactionSummary)
doGetTransactionOutcome pbs transHash = do
        bsp <- loadPBS pbs
        return $! (bspTransactionOutcomes bsp) ^? ix transHash

doSetTransactionOutcomes :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> [TransactionSummary] -> m PersistentBlockState
doSetTransactionOutcomes pbs transList = do
        bsp <- loadPBS pbs
        storePBS pbs bsp{bspTransactionOutcomes = Transactions.transactionOutcomesFromList transList}

doNotifyExecutionCost :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> Amount -> m PersistentBlockState
doNotifyExecutionCost pbs amnt = do
        bsp <- loadPBS pbs
        storePBS pbs bsp{bspBank = bspBank bsp & Rewards.executionCost +~ amnt}

doNotifyIdentityIssuerCredential :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> ID.IdentityProviderIdentity -> m PersistentBlockState
doNotifyIdentityIssuerCredential pbs idk = do
        bsp <- loadPBS pbs
        storePBS pbs bsp{bspBank = bspBank bsp & (Rewards.identityIssuersRewards . at' idk . non 0) +~ 1}

doGetExecutionCost :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> m Amount
doGetExecutionCost pbs = (^. Rewards.executionCost) . bspBank <$> loadPBS pbs

doGetSpecialOutcomes :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> m [Transactions.SpecialTransactionOutcome]
doGetSpecialOutcomes pbs = (^. to bspTransactionOutcomes . Transactions.outcomeSpecial) <$> loadPBS pbs

doGetOutcomes :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> m (Vec.Vector TransactionSummary)
doGetOutcomes pbs = (^. to bspTransactionOutcomes . to Transactions.outcomeValues) <$> loadPBS pbs


doAddSpecialTransactionOutcome :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> Transactions.SpecialTransactionOutcome -> m PersistentBlockState
doAddSpecialTransactionOutcome pbs !o = do
        bsp <- loadPBS pbs
        storePBS pbs $! bsp{bspTransactionOutcomes = bspTransactionOutcomes bsp & Transactions.outcomeSpecial %~ (o:)}

doUpdateBirkParameters :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> PersistentBirkParameters -> m PersistentBlockState
doUpdateBirkParameters pbs newBirk = do
        bsp <- loadPBS pbs
        storePBS pbs bsp{bspBirkParameters = newBirk}

doSetElectionDifficulty :: (MonadIO m, MonadBlobStore m BlobRef) => PersistentBlockState -> ElectionDifficulty -> m PersistentBlockState
doSetElectionDifficulty pbs d = do
        bsp <- loadPBS pbs
        storePBS pbs bsp{bspBirkParameters = bspBirkParameters bsp & birkElectionDifficulty .~ d}

data PersistentBlockStateContext = PersistentBlockStateContext {
    pbscModuleCache :: IORef ModuleCache,
    pbscBlobStore :: BlobStore
}

instance HasModuleCache PersistentBlockStateContext where
    moduleCache = pbscModuleCache

instance HasBlobStore PersistentBlockStateContext where
    blobStore = pbscBlobStore

newtype PersistentBlockStateMonad r m a = PersistentBlockStateMonad { runPersistentBlockStateMonad :: m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader r, MonadLogger)

type instance BlockStatePointer PersistentBlockState = BlobRef BlockStatePointers

instance BlockStateTypes (PersistentBlockStateMonad r m) where
    type BlockState (PersistentBlockStateMonad r m) = PersistentBlockState
    type UpdatableBlockState (PersistentBlockStateMonad r m) = PersistentBlockState
    type BirkParameters (PersistentBlockStateMonad r m) = PersistentBirkParameters
    type Bakers (PersistentBlockStateMonad r m) = PersistentBakers

instance (MonadIO m, MonadReader r m, HasBlobStore r) => BirkParametersOperations (PersistentBlockStateMonad r m) where

    getSeedState bps = return $ _birkSeedState bps

    updateBirkParametersForNewEpoch seedState bps = do
        currentBakers <- makeBufferedRef $ bps ^. birkCurrentBakers
        return $ bps &
            birkSeedState .~ seedState &
            -- use stake distribution saved from the former epoch for leader election
            birkLotteryBakers .~ (bps ^. birkPrevEpochBakers) &
            -- save the stake distribution from the end of the epoch
            birkPrevEpochBakers .~ currentBakers

    getElectionDifficulty = return . _birkElectionDifficulty

    getCurrentBakers = return . _birkCurrentBakers

    getLotteryBakers = loadBufferedRef . _birkLotteryBakers

    updateSeedState f bps = return $ bps & birkSeedState %~ f

instance (MonadIO m, HasModuleCache r, HasBlobStore r, MonadReader r m) => BlockStateQuery (PersistentBlockStateMonad r m) where
    getModule = doGetModule
    getAccount = doGetAccount
    getContractInstance = doGetInstance
    getModuleList = doGetModuleList
    getAccountList = doAccountList
    getContractInstanceList = doContractInstanceList
    getBlockBirkParameters = doGetBlockBirkParameters
    getRewardStatus = doGetRewardStatus
    getTransactionOutcome = doGetTransactionOutcome
    getSpecialOutcomes = doGetSpecialOutcomes
    getOutcomes = doGetOutcomes
    {-# INLINE getModule #-}
    {-# INLINE getAccount #-}
    {-# INLINE getContractInstance #-}
    {-# INLINE getModuleList #-}
    {-# INLINE getAccountList #-}
    {-# INLINE getContractInstanceList #-}
    {-# INLINE getBlockBirkParameters #-}
    {-# INLINE getRewardStatus #-}
    {-# INLINE getTransactionOutcome #-}
    {-# INLINE getOutcomes #-}
    {-# INLINE getSpecialOutcomes #-}

instance (MonadIO m, MonadReader r m, HasBlobStore r) => BakerQuery (PersistentBlockStateMonad r m) where

  getBakerStake bs bid = fmap snd <$> Trie.lookup bid (bs ^. bakerMap)

  getBakerFromKey bs k = return $ bs ^. bakersByKey . at' k

  getTotalBakerStake bs = return $ bs ^. bakerTotalStake

  getBakerInfo bs bid = Trie.lookup bid (bs ^. bakerMap) >>= \case
    Just (bInfoRef, _) -> Just <$> loadBufferedRef bInfoRef
    Nothing -> return Nothing

  getFullBakerInfos PersistentBakers{..} =
    mapM getFullInfo =<< Trie.toMap _bakerMap
    where getFullInfo (binfoRef, stake) = do
            binfo <- loadBufferedRef binfoRef
            return $ FullBakerInfo binfo stake

instance (MonadIO m, MonadReader r m, HasBlobStore r, HasModuleCache r) => BlockStateOperations (PersistentBlockStateMonad r m) where
    bsoGetModule = doGetModule
    bsoGetAccount = doGetAccount
    bsoGetInstance = doGetInstance
    bsoRegIdExists = doRegIdExists
    bsoPutNewAccount = doPutNewAccount
    bsoPutNewInstance = doPutNewInstance
    bsoPutNewModule = doPutNewModule
    bsoTryGetLinkedExpr = doTryGetLinkedExpr
    bsoPutLinkedExpr = doPutLinkedExpr
    bsoTryGetLinkedContract = doTryGetLinkedContract
    bsoPutLinkedContract = doPutLinkedContract
    bsoModifyAccount = doModifyAccount
    bsoModifyInstance = doModifyInstance
    bsoNotifyExecutionCost = doNotifyExecutionCost
    bsoNotifyIdentityIssuerCredential = doNotifyIdentityIssuerCredential
    bsoGetExecutionCost = doGetExecutionCost
    bsoGetBlockBirkParameters = doGetBlockBirkParameters
    bsoAddBaker = doAddBaker
    bsoUpdateBaker = doUpdateBaker
    bsoRemoveBaker = doRemoveBaker
    bsoSetInflation = doSetInflation
    bsoMint = doMint
    bsoDecrementCentralBankGTU = doDecrementCentralBankGTU
    bsoDelegateStake = doDelegateStake
    bsoGetIdentityProvider = doGetIdentityProvider
    bsoGetCryptoParams = doGetCryptoParams
    bsoSetTransactionOutcomes = doSetTransactionOutcomes
    bsoAddSpecialTransactionOutcome = doAddSpecialTransactionOutcome
    bsoUpdateBirkParameters = doUpdateBirkParameters
    bsoSetElectionDifficulty = doSetElectionDifficulty
    {-# INLINE bsoGetModule #-}
    {-# INLINE bsoGetAccount #-}
    {-# INLINE bsoGetInstance #-}
    {-# INLINE bsoRegIdExists #-}
    {-# INLINE bsoPutNewAccount #-}
    {-# INLINE bsoPutNewInstance #-}
    {-# INLINE bsoPutNewModule #-}
    {-# INLINE bsoTryGetLinkedExpr #-}
    {-# INLINE bsoPutLinkedExpr #-}
    {-# INLINE bsoTryGetLinkedContract #-}
    {-# INLINE bsoPutLinkedContract #-}
    {-# INLINE bsoModifyAccount #-}
    {-# INLINE bsoModifyInstance #-}
    {-# INLINE bsoNotifyExecutionCost #-}
    {-# INLINE bsoNotifyIdentityIssuerCredential #-}
    {-# INLINE bsoGetExecutionCost #-}
    {-# INLINE bsoGetBlockBirkParameters #-}
    {-# INLINE bsoAddBaker #-}
    {-# INLINE bsoUpdateBaker #-}
    {-# INLINE bsoRemoveBaker #-}
    {-# INLINE bsoSetInflation #-}
    {-# INLINE bsoMint #-}
    {-# INLINE bsoDecrementCentralBankGTU #-}
    {-# INLINE bsoDelegateStake #-}
    {-# INLINE bsoGetIdentityProvider #-}
    {-# INLINE bsoGetCryptoParams #-}
    {-# INLINE bsoSetTransactionOutcomes #-}
    {-# INLINE bsoAddSpecialTransactionOutcome #-}
    {-# INLINE bsoUpdateBirkParameters #-}
    {-# INLINE bsoSetElectionDifficulty #-}

instance (MonadIO m, MonadReader r m, HasBlobStore r, HasModuleCache r) => BlockStateStorage (PersistentBlockStateMonad r m) where
    {-# INLINE thawBlockState #-}
    thawBlockState pbs = do
            bsp <- loadPBS pbs
            pbsp <- makeBufferedRef bsp {
                        bspBank = bspBank bsp & Rewards.executionCost .~ 0 & Rewards.identityIssuersRewards .~ HM.empty
                    }
            liftIO $ newIORef $! pbsp

    {-# INLINE freezeBlockState #-}
    freezeBlockState pbs = return pbs

    {-# INLINE dropUpdatableBlockState #-}
    dropUpdatableBlockState pbs = liftIO $ writeIORef pbs (error "Block state dropped")

    {-# INLINE purgeBlockState #-}
    purgeBlockState pbs = liftIO $ writeIORef pbs (error "Block state purged")

    {-# INLINE archiveBlockState #-}
    archiveBlockState pbs = do
        inner <- liftIO $ readIORef pbs
        inner' <- uncacheBuffered inner
        liftIO $ writeIORef pbs inner'

    saveBlockState pbs = do
        inner <- liftIO $ readIORef pbs
        (inner', ref) <- flushBufferedRef inner
        liftIO $ writeIORef pbs inner'
        bs <- blobStore <$> ask
        liftIO $ flushBlobStore bs
        return ref
    
    loadBlockState = liftIO . newIORef . BRBlobbed
