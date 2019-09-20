{-# LANGUAGE RecordWildCards, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, GeneralizedNewtypeDeriving,
        TypeFamilies, BangPatterns, TemplateHaskell
 #-}

module Concordium.GlobalState.Persistent.BlockState where

import Data.Void
import Data.Proxy
import Data.Serialize
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Control.Monad.Reader.Class
import Control.Monad.IO.Class
import Lens.Micro.Platform

import qualified Concordium.Crypto.SHA256 as H
import qualified Concordium.Types.Acorn.Core as Core
import Concordium.Types.Acorn.Interfaces
import Concordium.Types.HashableTo
import Concordium.Types
import qualified Concordium.ID.Types as ID

import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.Trie as Trie
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Bakers
import qualified Concordium.GlobalState.Rewards as Rewards
import qualified Concordium.GlobalState.Persistent.Account as Account

type PersistentBlockState = BufferedRef BlockStatePointers

data BlockStatePointers = BlockStatePointers {
    bspAccounts :: !Account.Accounts,
    bspModules :: BufferedRef Modules,
    bspBank :: !Rewards.BankStatus,
    bspBirkParameters :: !BirkParameters -- TODO: Possibly store BirkParameters allowing for sharing
}

instance (MonadBlobStore m BlobRef) => BlobStorable m BlobRef BlockStatePointers where
    storeUpdate p bsp0@BlockStatePointers{..} = do
        (paccts, bspAccounts') <- storeUpdate p bspAccounts
        (pmods, bspModules') <- storeUpdate p bspModules
        let putBSP = do
                paccts
                pmods
                put bspBank
                put bspBirkParameters
        return (putBSP, bsp0 {bspAccounts = bspAccounts', bspModules = bspModules'})
    store p bsp = fst <$> storeUpdate p bsp
    load p = do
        maccts <- load p
        mmods <- load p
        bspBank <- get
        bspBirkParameters <- get
        return $! do
            bspAccounts <- maccts
            bspModules <- mmods
            return $! BlockStatePointers{..}


data PersistentModule = PersistentModule {
    pmInterface :: !(Interface Core.UA),
    pmValueInterface :: !(UnlinkedValueInterface Void),
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

instance (MonadBlobStore m BlobRef) => BlobStorable m BlobRef Modules where
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

data ModuleCache = ModuleCache {
    _cachedLinkedDefs :: HM.HashMap (Core.ModuleRef, Core.Name) (LinkedExpr Void),
    _cachedLinkedContracts :: HM.HashMap (Core.ModuleRef, Core.TyName) (LinkedContractValue Void)
}
makeLenses ''ModuleCache

class HasModuleCache a where
    moduleCache :: a -> IORef ModuleCache


doGetModule :: (MonadBlobStore m BlobRef) => PersistentBlockState -> Core.ModuleRef -> m (Maybe Module)
doGetModule s modRef = do
        bsp <- loadBufferedRef s
        mods <- loadBufferedRef (bspModules bsp)
        fmap persistentModuleToModule <$> Trie.lookup modRef (modules mods)

doGetModuleList :: (MonadBlobStore m BlobRef) => PersistentBlockState -> m [Core.ModuleRef]
doGetModuleList s = do
        bsp <- loadBufferedRef s
        mods <- loadBufferedRef (bspModules bsp)
        Trie.keys (modules mods)

doPutNewModule :: (MonadBlobStore m BlobRef) => PersistentBlockState
    -> Core.ModuleRef
    -> Interface Core.UA
    -> UnlinkedValueInterface Void
    -> Core.Module Core.UA
    -> m (Bool, PersistentBlockState)
doPutNewModule pbs mref pmInterface pmValueInterface pmSource = do
        bsp <- loadBufferedRef pbs
        mods <- loadBufferedRef (bspModules bsp)
        let
            newMod = PersistentModule{pmIndex = nextModuleIndex mods, ..}
            tryIns Nothing = return (True, Trie.Insert newMod)
            tryIns (Just _) = return (False, Trie.NoChange)
        (b, modules') <- Trie.adjust tryIns mref (modules mods)
        if b then do
            let
                newMods = mods {modules = modules', nextModuleIndex = nextModuleIndex mods + 1}
            return $! (True, BRMemory (bsp {bspModules = BRMemory newMods}))
        else
            return (False, pbs)

doTryGetLinkedExpr :: (MonadIO m, MonadReader r m, HasModuleCache r) => PersistentBlockState -> Core.ModuleRef -> Core.Name -> m (Maybe (LinkedExpr Void))
doTryGetLinkedExpr _ modRef n = do
        cache <- asks moduleCache >>= liftIO . readIORef
        return $! HM.lookup (modRef, n) (_cachedLinkedDefs cache)

doPutLinkedExpr :: (MonadIO m, MonadReader r m, HasModuleCache r) => PersistentBlockState -> Core.ModuleRef -> Core.Name -> LinkedExpr Void -> m PersistentBlockState
doPutLinkedExpr pbs modRef n !le = do
        cacheRef <- asks moduleCache
        liftIO $ modifyIORef' cacheRef (cachedLinkedDefs %~ HM.insert (modRef, n) le)
        return pbs

doTryGetLinkedContract :: (MonadIO m, MonadReader r m, HasModuleCache r) => PersistentBlockState -> Core.ModuleRef -> Core.TyName -> m (Maybe (LinkedContractValue Void))
doTryGetLinkedContract _ modRef n = do
        cache <- asks moduleCache >>= liftIO . readIORef
        return $! HM.lookup (modRef, n) (_cachedLinkedContracts cache)

doPutLinkedContract :: (MonadIO m, MonadReader r m, HasModuleCache r) => PersistentBlockState -> Core.ModuleRef -> Core.TyName -> LinkedContractValue Void -> m PersistentBlockState
doPutLinkedContract pbs modRef n !lc = do
        cacheRef <- asks moduleCache
        liftIO $ modifyIORef' cacheRef (cachedLinkedContracts %~ HM.insert (modRef, n) lc)
        return pbs

doGetBlockBirkParameters :: (MonadBlobStore m BlobRef) => PersistentBlockState -> m BirkParameters
doGetBlockBirkParameters pbs = bspBirkParameters <$> loadBufferedRef pbs

doAddBaker :: (MonadBlobStore m BlobRef) => PersistentBlockState -> BakerCreationInfo -> m (BakerId, PersistentBlockState)
doAddBaker pbs binfo = do
        bsp <- loadBufferedRef pbs
        let (bid, newBakers) = createBaker binfo (bspBirkParameters bsp ^. birkBakers)
        return (bid, BRMemory (bsp {bspBirkParameters = bspBirkParameters bsp & birkBakers .~ newBakers}))

doUpdateBaker :: (MonadBlobStore m BlobRef) => PersistentBlockState -> BakerUpdate -> m PersistentBlockState
doUpdateBaker pbs bupdate = do
        bsp <- loadBufferedRef pbs
        return $ BRMemory (bsp {bspBirkParameters = bspBirkParameters bsp & birkBakers %~ updateBaker bupdate})

doRemoveBaker :: (MonadBlobStore m BlobRef) => PersistentBlockState -> BakerId -> m (Bool, PersistentBlockState)
doRemoveBaker pbs bid = do
        bsp <- loadBufferedRef pbs
        let (rv, newBakers) = removeBaker bid (bspBirkParameters bsp ^. birkBakers)
        return (rv, BRMemory (bsp {bspBirkParameters = bspBirkParameters bsp & birkBakers .~ newBakers}))

doGetRewardStatus :: (MonadBlobStore m BlobRef) => PersistentBlockState -> m Rewards.BankStatus
doGetRewardStatus pbs = bspBank <$> loadBufferedRef pbs

doSetInflation :: (MonadBlobStore m BlobRef) => PersistentBlockState -> Amount -> m PersistentBlockState
doSetInflation pbs amount = do
        bsp <- loadBufferedRef pbs
        return $ BRMemory (bsp {bspBank = bspBank bsp & Rewards.mintedGTUPerSlot .~ amount})

doMint :: (MonadBlobStore m BlobRef) => PersistentBlockState -> Amount -> m (Amount, PersistentBlockState)
doMint pbs amount = do
        bsp <- loadBufferedRef pbs
        let newBank = bspBank bsp & (Rewards.totalGTU +~ amount) . (Rewards.centralBankGTU +~ amount)
        return (newBank ^. Rewards.centralBankGTU, BRMemory (bsp {bspBank = newBank}))

doDecrementCentralBankGTU :: (MonadBlobStore m BlobRef) => PersistentBlockState -> Amount -> m (Amount, PersistentBlockState)
doDecrementCentralBankGTU pbs amount = do
        bsp <- loadBufferedRef pbs
        let newBank = bspBank bsp & Rewards.centralBankGTU -~ amount
        return (newBank ^. Rewards.centralBankGTU, BRMemory (bsp {bspBank = newBank}))

doGetAccount :: (MonadBlobStore m BlobRef) => PersistentBlockState -> AccountAddress -> m (Maybe Account)
doGetAccount pbs addr = do
        bsp <- loadBufferedRef pbs
        Account.getAccount addr (bspAccounts bsp)

doAccountList :: (MonadBlobStore m BlobRef) => PersistentBlockState -> m [AccountAddress]
doAccountList pbs = do
        bsp <- loadBufferedRef pbs
        Account.accountAddresses (bspAccounts bsp)

doRegIdExists :: (MonadBlobStore m BlobRef) => PersistentBlockState -> ID.CredentialRegistrationID -> m Bool
doRegIdExists pbs regid = do
        bsp <- loadBufferedRef pbs
        fst <$> Account.regIdExists regid (bspAccounts bsp)

doPutNewAccount :: (MonadBlobStore m BlobRef) => PersistentBlockState -> Account -> m (Bool, PersistentBlockState)
doPutNewAccount pbs acct = do 
        bsp <- loadBufferedRef pbs
        (res, accts') <- Account.putNewAccount acct (bspAccounts bsp)
        return (res, BRMemory (bsp {bspAccounts = accts'}))
    
doModifyAccount :: (MonadBlobStore m BlobRef) => PersistentBlockState -> AccountUpdate -> m PersistentBlockState
doModifyAccount pbs aUpd@AccountUpdate{..} = do
        bsp <- loadBufferedRef pbs
        -- Do the update to the account
        (mbalinfo, accts1) <- Account.updateAccount upd _auAddress (bspAccounts bsp)
        -- If we deploy a credential, record it
        accts2 <- case _auCredential of
            Just cdi -> Account.recordRegId (ID.cdvRegId cdi) accts1
            Nothing -> return accts1
        -- If the amount is changed update the delegate stake
        let birkParams1 = case (_auAmount, mbalinfo) of
                (Just newAmount, Just (delegate, oldAmount)) ->
                    bspBirkParameters bsp & birkBakers %~ modifyStake delegate (amountDiff newAmount oldAmount)  
                _ -> bspBirkParameters bsp
        return $! BRMemory (bsp {bspAccounts = accts2, bspBirkParameters = birkParams1})
    where
        upd oldAccount = return ((oldAccount ^. accountStakeDelegate, oldAccount ^. accountAmount), updateAccount aUpd oldAccount)

{-
doDelegateStake :: (MonadBlobStore m BlobRef) => PersistentBlockState -> AccountAddress -> Maybe BakerId -> m (Bool, PersistentBlockState)
doDelegateStake pbs aaddr target
-}