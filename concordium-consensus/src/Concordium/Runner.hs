{-# LANGUAGE LambdaCase, FlexibleContexts, ScopedTypeVariables, RecordWildCards, FlexibleInstances, MultiParamTypeClasses #-}
module Concordium.Runner where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad.Trans.State hiding (get, put)
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import Data.Time.Clock
import Data.ByteString as BS
import Data.Serialize
import Data.IORef

import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Block
import Concordium.GlobalState.BlockState(BlockState, LogTransferMethod)
import Concordium.GlobalState.Transactions
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Basic.Block
import Concordium.GlobalState.Persistent.TreeState(PersistentBlockPointer, _bpBlock)
import Concordium.GlobalState.Persistent.BlockState(emptyModuleCache)
import Concordium.GlobalState.Persistent.BlobStore(createTempBlobStore,destroyTempBlobStore)
import Concordium.TimeMonad
import Concordium.Birk.Bake
import Concordium.Kontrol
import Concordium.Skov
-- import Concordium.Skov.Update
import Concordium.Skov.Hooks
import Concordium.Afgjort.Finalize
import Concordium.Afgjort.Buffer
import Concordium.Logger
import Concordium.Getters

data SyncRunner = SyncRunner {
    syncBakerIdentity :: BakerIdentity,
    syncState :: MVar SkovBufferedHookedState,
    syncBakerThread :: MVar ThreadId,
    syncLogMethod :: LogMethod IO,
    syncLogTransferMethod :: Maybe (LogTransferMethod IO),
    syncCallback :: SimpleOutMessage -> IO (),
    syncFinalizationCatchUpActive :: MVar (Maybe (IORef Bool)),
    syncPersistentContext :: PersistentContext
}

createPersistentContext :: IO PersistentContext
createPersistentContext = do
    pcBlobStore <- createTempBlobStore
    pcModuleCache <- newIORef emptyModuleCache
    return PersistentContext{..}

destroyPersistentContext :: PersistentContext -> IO ()
destroyPersistentContext PersistentContext{..} = do
    destroyTempBlobStore pcBlobStore
    writeIORef pcModuleCache emptyModuleCache

bakerFinalizationInstance :: BakerIdentity -> FinalizationInstance
bakerFinalizationInstance bkr = FinalizationInstance (bakerSignKey bkr) (bakerElectionKey bkr)

syncPersistentFinalizationLoggedContext :: SyncRunner -> PersistentFinalizationLoggedContext
syncPersistentFinalizationLoggedContext SyncRunner{..} = PersistentFinalizationLoggedContext {
        pflcBlobStore = pcBlobStore syncPersistentContext,
        pflcModuleCache = pcModuleCache syncPersistentContext,
        pflcFinalizationInstance = bakerFinalizationInstance syncBakerIdentity,
        pflcLogMethod = syncLogTransferMethod
    }

instance SkovStateQueryable SyncRunner (SkovQueryM PersistentContext SkovBufferedHookedState IO) where
    runStateQuery sr a = readMVar (syncState sr) >>= evalSkovQueryM a (syncPersistentContext sr)

data SimpleOutMessage
    = SOMsgNewBlock PersistentBlockPointer
    | SOMsgFinalization FinalizationPseudoMessage
    | SOMsgFinalizationRecord FinalizationRecord

-- |Run a computation, atomically using the state.  If the computation fails with an
-- exception, the state is restored to the original state, ensuring that the lock is released.
runWithStateLog :: MVar s -> LogMethod IO -> (s -> LogIO (a, s)) -> IO a
{-# INLINE runWithStateLog #-}
runWithStateLog mvState logm a = bracketOnError (takeMVar mvState) (tryPutMVar mvState) $ \state0 -> do
        (ret, state') <- runLoggerT (a state0) logm
        putMVar mvState state'
        return ret

asyncNotify :: MVar SkovBufferedHookedState -> LogMethod IO -> (FinalizationMessage -> IO ()) -> NotifyEvent -> IO ()
asyncNotify mvState logm cbk ne@(timeout, _) = void $ forkIO $ do
        now <- getCurrentTime
        let delay = diffUTCTime timeout now
        when (delay > 0) $ threadDelay (truncate $ delay * 1e6)
        mmsg <- runWithStateLog mvState logm (runStateT $ notifyBuffer ne)
        forM_ mmsg cbk

asyncTriggerFinalizationCatchUp :: SyncRunner -> Maybe NominalDiffTime -> IO ()
asyncTriggerFinalizationCatchUp SyncRunner{..} Nothing =
        swapMVar syncFinalizationCatchUpActive Nothing >>=
        mapM_ (\ref -> writeIORef ref False) -- Tell any thread waiting to trigger finalization catch-up to abort
asyncTriggerFinalizationCatchUp SyncRunner{..} (Just delay) = when (delay > 0) $ do
        myRef <- newIORef True
        swapMVar syncFinalizationCatchUpActive (Just myRef) >>= mapM_ (\ref -> writeIORef ref False)
        let loop n = do
                threadDelay (n * truncate (delay * 1e6))
                continue <- readIORef myRef
                when continue $ do
                    st <- readMVar syncState
                    let mFinMsg = finalizationCatchUpMessage (bakerFinalizationInstance syncBakerIdentity) st
                    forM_ mFinMsg $ \msg -> do
                        syncLogMethod Skov LLDebug "Sending finalization catch-up message"
                        syncCallback (SOMsgFinalization msg)
                    loop (n + 1)
        void $ forkIO $ loop 1



-- |Make a 'SyncRunner' without starting a baker thread.
makeSyncRunner :: forall m. LogMethod IO ->
                  Maybe (LogTransferMethod IO) ->
                  BakerIdentity ->
                  RuntimeParameters ->
                  GenesisData ->
                  BlockState (SkovBufferedM m) -> (SimpleOutMessage -> IO ()) -> IO SyncRunner
makeSyncRunner syncLogMethod syncLogTransferMethod syncBakerIdentity rtParams gen initBS syncCallback = do
        let
            syncFinalizationInstance = bakerFinalizationInstance syncBakerIdentity
            sfs0 = initialSkovBufferedHookedState syncFinalizationInstance rtParams gen initBS
        syncState <- newMVar sfs0
        syncBakerThread <- newEmptyMVar
        syncFinalizationCatchUpActive <- newMVar Nothing
        syncPersistentContext <- createPersistentContext
        return $ SyncRunner{..}

-- |Start the baker thread for a 'SyncRunner'.
startSyncRunner :: SyncRunner -> IO ()
startSyncRunner sr@SyncRunner{..} = do
        let
            runBaker = bakeLoop 0 `finally` syncLogMethod Runner LLInfo "Exiting baker thread"
            bakeLoop lastSlot = do
                (mblock, sfs', evs, curSlot) <- runWithStateLog syncState syncLogMethod (\sfs -> do
                        let bake = do
                                curSlot <- getCurrentSlot
                                mblock <- if (curSlot > lastSlot) then bakeForSlot syncBakerIdentity curSlot else return Nothing
                                return (mblock, curSlot)
                        ((mblock, curSlot), sfs', evs) <-
                          runSkovBufferedHookedLoggedM bake (syncPersistentFinalizationLoggedContext sr) sfs
                        return ((mblock, sfs', evs, curSlot), sfs'))
                forM_ mblock $ syncCallback . SOMsgNewBlock
                let
                    handleFinalizationOutputEvent (BroadcastFinalizationMessage fmsg) = syncCallback (SOMsgFinalization (FPMMessage fmsg))
                    handleFinalizationOutputEvent (BroadcastFinalizationRecord frec) = syncCallback (SOMsgFinalizationRecord frec)
                forM_ (extractFinalizationOutputEvents evs) handleFinalizationOutputEvent
                forM_ (extractNotifyEvents evs) (asyncNotify syncState syncLogMethod (syncCallback . SOMsgFinalization . FPMMessage))
                forM_ (extractCatchUpTimer evs) (asyncTriggerFinalizationCatchUp sr)
                delay <- evalSkovQueryM (do
                    ttns <- timeUntilNextSlot
                    curSlot' <- getCurrentSlot
                    return $! if curSlot == curSlot' then truncate (ttns * 1e6) else 0) syncPersistentContext sfs'
                when (delay > 0) $ threadDelay delay
                bakeLoop curSlot
        _ <- forkIO $ do
            tid <- myThreadId
            putRes <- tryPutMVar syncBakerThread tid
            if putRes then do
                syncLogMethod Runner LLInfo "Starting baker thread"
                runBaker
            else
                syncLogMethod Runner LLInfo "Starting baker thread aborted: baker is already running"
        return ()

-- |Stop the baker thread for a 'SyncRunner'.
stopSyncRunner :: SyncRunner -> IO ()
stopSyncRunner SyncRunner{..} = mask_ $ tryTakeMVar syncBakerThread >>= \case
        Nothing -> return ()
        Just thrd -> killThread thrd

shutdownSyncRunner :: SyncRunner -> IO ()
shutdownSyncRunner sr@SyncRunner{..} = do
        stopSyncRunner sr
        destroyPersistentContext syncPersistentContext

runSkovBufferedMWithStateLog :: SyncRunner -> SkovBufferedHookedLoggedM (LoggerT IO) a -> IO (a, [FinalizationOutputEvent])
runSkovBufferedMWithStateLog sr@SyncRunner{..} a = do
     (ret, evts) <- runWithStateLog syncState syncLogMethod (\sfs -> 
         (\(ret, sfs', evs) -> ((ret, evs), sfs')) <$> runSkovBufferedHookedLoggedM a (syncPersistentFinalizationLoggedContext sr) sfs)
     forM_ (extractNotifyEvents evts) $ asyncNotify syncState syncLogMethod (syncCallback . SOMsgFinalization . FPMMessage)
     forM_ (extractCatchUpTimer evts) (asyncTriggerFinalizationCatchUp sr)
     return (ret, extractFinalizationOutputEvents evts)

syncReceiveBlock :: SyncRunner -> PendingBlock -> IO (UpdateResult, [FinalizationOutputEvent])
syncReceiveBlock syncRunner block = runSkovBufferedMWithStateLog syncRunner (storeBlock block)

syncReceiveTransaction :: SyncRunner -> Transaction -> IO (UpdateResult, [FinalizationOutputEvent])
syncReceiveTransaction syncRunner trans = runSkovBufferedMWithStateLog syncRunner (receiveTransaction trans)

syncReceiveFinalizationMessage :: SyncRunner -> FinalizationPseudoMessage -> IO (UpdateResult, [FinalizationOutputEvent])
syncReceiveFinalizationMessage syncRunner finMsg = runSkovBufferedMWithStateLog syncRunner (receiveFinalizationPseudoMessage finMsg)

syncReceiveFinalizationRecord :: SyncRunner -> FinalizationRecord -> IO (UpdateResult, [FinalizationOutputEvent])
syncReceiveFinalizationRecord syncRunner finRec = runSkovBufferedMWithStateLog syncRunner (finalizeBlock finRec)

syncHookTransaction :: SyncRunner -> TransactionHash -> IO HookResult
-- hookQueryTransaction does not generate any events, so it is safe to drop them.
syncHookTransaction syncRunner th = fst <$> runSkovBufferedMWithStateLog syncRunner (hookQueryTransaction th)

data SyncPassiveRunner = SyncPassiveRunner {
    syncPState :: MVar SkovPassiveHookedState,
    syncPLogMethod :: LogMethod IO,
    syncPPersistentContext :: PersistentContext
}

instance SkovStateQueryable SyncPassiveRunner (SkovQueryM PersistentContext SkovPassiveHookedState IO) where
    runStateQuery sr a = readMVar (syncPState sr) >>= evalSkovQueryM a (syncPPersistentContext sr)


-- |Make a 'SyncPassiveRunner', which does not support a baker thread.
makeSyncPassiveRunner :: forall m. LogMethod IO -> RuntimeParameters -> GenesisData -> BlockState (SkovPassiveHookedM m) -> IO SyncPassiveRunner
makeSyncPassiveRunner syncPLogMethod rtParams gen initBS = do
        syncPState <- newMVar $ initialSkovPassiveHookedState rtParams gen initBS
        syncPPersistentContext <- createPersistentContext
        return $ SyncPassiveRunner{..}

shutdownSyncPassiveRunner :: SyncPassiveRunner -> IO ()
shutdownSyncPassiveRunner SyncPassiveRunner{..} = destroyPersistentContext syncPPersistentContext

runSkovPassiveMWithStateLog :: SyncPassiveRunner -> SkovPassiveHookedM LogIO a -> IO a
runSkovPassiveMWithStateLog SyncPassiveRunner{..} =
        runWithStateLog syncPState syncPLogMethod . flip runSkovPassiveHookedM syncPPersistentContext

syncPassiveReceiveBlock :: SyncPassiveRunner -> PendingBlock -> IO UpdateResult
syncPassiveReceiveBlock spr block = runSkovPassiveMWithStateLog spr (storeBlock block)

syncPassiveReceiveTransaction :: SyncPassiveRunner -> Transaction -> IO UpdateResult
syncPassiveReceiveTransaction spr trans = runSkovPassiveMWithStateLog spr (receiveTransaction trans)

syncPassiveReceiveFinalizationMessage :: SyncPassiveRunner -> FinalizationPseudoMessage -> BS.ByteString -> IO UpdateResult
syncPassiveReceiveFinalizationMessage spr pmsg pmsgBS = runSkovPassiveMWithStateLog spr (passiveReceiveFinalizationPseudoMessage pmsg pmsgBS)

syncPassiveReceiveFinalizationRecord :: SyncPassiveRunner -> FinalizationRecord -> IO UpdateResult
syncPassiveReceiveFinalizationRecord spr finRec = runSkovPassiveMWithStateLog spr (finalizeBlock finRec)

syncPassiveHookTransaction :: SyncPassiveRunner -> TransactionHash -> IO HookResult
-- hookQueryTransaction does not generate any events, so it is safe to drop them.
syncPassiveHookTransaction syncRunner th = runSkovPassiveMWithStateLog syncRunner (hookQueryTransaction th)


data InMessage src =
    MsgShutdown
    | MsgBlockReceived src !BS.ByteString
    | MsgTransactionReceived !BS.ByteString
    | MsgFinalizationReceived src !BS.ByteString
    | MsgFinalizationRecordReceived src !BS.ByteString
    | MsgCatchUpStatusReceived src !BS.ByteString

data OutMessage peer = 
    MsgNewBlock !BS.ByteString
    | MsgFinalization !BS.ByteString
    | MsgFinalizationRecord !BS.ByteString
    | MsgCatchUpRequired peer
    | MsgDirectedBlock peer !BS.ByteString
    | MsgDirectedFinalizationRecord peer !BS.ByteString
    | MsgDirectedCatchUpStatus peer !BS.ByteString

-- |This is provided as a compatibility wrapper for the test runners.
makeAsyncRunner :: forall m source. LogMethod IO ->
                   Maybe (LogTransferMethod IO) ->
                   BakerIdentity ->
                   RuntimeParameters ->
                   GenesisData ->
                   BlockState (SkovBufferedM m) ->
                   IO (Chan (InMessage source), Chan (OutMessage source), MVar SkovBufferedHookedState, PersistentContext)
makeAsyncRunner logm logt bkr rtParams gen initBS = do
        logm Runner LLInfo "Starting baker"
        inChan <- newChan
        outChan <- newChan
        let somHandler = writeChan outChan . simpleToOutMessage
        sr <- makeSyncRunner logm logt bkr rtParams gen initBS somHandler
        startSyncRunner sr
        let
            msgLoop = readChan inChan >>= \case
                MsgShutdown -> stopSyncRunner sr
                MsgBlockReceived src blockBS -> do
                    now <- currentTime
                    case runGet (getBlock now) blockBS of
                        Right (NormalBlock block) -> do
                            (res, evts) <- syncReceiveBlock sr $ makePendingBlock block now
                            forM_ evts $ handleMessage
                            handleResult src res
                        _ -> return ()
                    msgLoop
                MsgTransactionReceived transBS -> do
                    now <- currentTime
                    case runGet (getVerifiedTransaction now) transBS of
                        Right trans -> do
                            (_, evts) <- syncReceiveTransaction sr trans
                            forM_ evts $ handleMessage
                        _ -> return ()
                    msgLoop
                MsgFinalizationReceived src bs -> do
                    case runGet get bs of
                        Right finMsg -> do
                            (res, evts) <- syncReceiveFinalizationMessage sr finMsg
                            forM_ evts $ handleMessage
                            handleResult src res
                        _ -> return ()
                    msgLoop
                MsgFinalizationRecordReceived src finRecBS -> do
                    case runGet get finRecBS of
                        Right finRec -> do
                            (res, evts) <- syncReceiveFinalizationRecord sr finRec
                            forM_ evts $ handleMessage
                            handleResult src res
                        _ -> return ()
                    msgLoop
                MsgCatchUpStatusReceived src cuBS -> do
                    case runGet get cuBS of
                        Right cu -> do
                            res <- handleCatchUpStatus sr cu
                            case res of
                                Right (d, flag) -> do
                                    let
                                        send (Left fr) = writeChan outChan (MsgDirectedFinalizationRecord src (encode fr))
                                        send (Right b) = writeChan outChan (MsgDirectedBlock src (runPut (putBlock (_bpBlock b))))
                                    forM_ d $ \(frbs, rcus) -> do
                                        mapM_ send frbs
                                        writeChan outChan (MsgDirectedCatchUpStatus src (encode rcus))
                                    when flag $ writeChan outChan (MsgCatchUpRequired src)
                                _ -> return ()
                        _ -> return ()
                    msgLoop
            handleMessage (BroadcastFinalizationMessage fmsg) = writeChan outChan (MsgFinalization $ runPut $ put fmsg)
            handleMessage (BroadcastFinalizationRecord frec) = writeChan outChan (MsgFinalizationRecord $ runPut $ put frec)
            handleResult src ResultPendingBlock = writeChan outChan (MsgCatchUpRequired src)
            handleResult src ResultPendingFinalization = writeChan outChan (MsgCatchUpRequired src)
            handleResult _ _ = return ()
        _ <- forkIO msgLoop
        return (inChan, outChan, syncState sr, syncPersistentContext sr)
    where
        simpleToOutMessage (SOMsgNewBlock block) = MsgNewBlock $ runPut $ putBlock block
        simpleToOutMessage (SOMsgFinalization finMsg) = MsgFinalization $ runPut $ put finMsg
        simpleToOutMessage (SOMsgFinalizationRecord finRec) = MsgFinalizationRecord $ runPut $ put finRec
