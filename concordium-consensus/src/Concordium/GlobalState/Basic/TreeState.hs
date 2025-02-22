{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Concordium.GlobalState.Basic.TreeState where

import Lens.Micro.Platform
import Concordium.Utils
import Data.List (intercalate, partition)
import Data.Foldable
import Control.Monad.State
import Control.Exception
import Data.Functor.Identity

import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import qualified Data.PQueue.Prio.Min as MPQ
import qualified Data.Set as Set

import Concordium.GlobalState.Types
import Concordium.GlobalState.Basic.BlockPointer
import Concordium.GlobalState.Block
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockPointer
import qualified Concordium.GlobalState.BlockState as BS
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.TransactionTable
import Concordium.GlobalState.PurgeTransactions
import Concordium.GlobalState.Statistics (ConsensusStatistics, initialConsensusStatistics)
import qualified Concordium.GlobalState.TreeState as TS
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Transactions
import Concordium.Types.Updates
import Concordium.GlobalState.AccountTransactionIndex

-- |Datatype representing an in-memory tree state.
-- The first type parameter, @pv@, is the protocol version.
-- The second type parameter, @bs@, is the type of the block state.
data SkovData (pv :: ProtocolVersion) bs = SkovData {
    -- |Map of all received blocks by hash.
    _blockTable :: !(HM.HashMap BlockHash (TS.BlockStatus (BasicBlockPointer pv bs) PendingBlock)),
    -- |Table of blocks finalized by height.
    _finalizedByHeightTable :: !(HM.HashMap BlockHeight (BasicBlockPointer pv bs)),
    -- |Map of (possibly) pending blocks by hash
    _possiblyPendingTable :: !(HM.HashMap BlockHash [PendingBlock]),
    -- |Priority queue of pairs of (block, parent) hashes where the block is (possibly) pending its parent, by block slot
    _possiblyPendingQueue :: !(MPQ.MinPQueue Slot (BlockHash, BlockHash)),
    -- |List of finalization records with the blocks that they finalize, starting from genesis
    _finalizationList :: !(Seq.Seq (FinalizationRecord, BasicBlockPointer pv bs)),
    -- |Branches of the tree by height above the last finalized block
    _branches :: !(Seq.Seq [BasicBlockPointer pv bs]),
    -- |Genesis data
    _genesisData :: !(GenesisData pv),
    -- |Block pointer to genesis block
    _genesisBlockPointer :: !(BasicBlockPointer pv bs),
    -- |Current focus block
    _focusBlock :: !(BasicBlockPointer pv bs),
    -- |Pending transaction table
    _pendingTransactions :: !PendingTransactionTable,
    -- |Transaction table
    _transactionTable :: !TransactionTable,
    -- |Consensus statistics
    _statistics :: !ConsensusStatistics,
    -- |Runtime parameters
    _runtimeParameters :: !RuntimeParameters,
    -- |Transaction table purge counter
    _transactionTablePurgeCounter :: !Int
}
makeLenses ''SkovData

instance IsProtocolVersion pv => Show (SkovData pv bs) where
    show SkovData{..} = "Finalized: " ++ intercalate "," (take 6 . show . bpHash . snd <$> toList _finalizationList) ++ "\n" ++
        "Branches: " ++ intercalate "," ( ('[':) . (++"]") . intercalate "," . map (take 6 . show . bpHash) <$> toList _branches)

-- |Initial skov data with default runtime parameters (block size = 10MB).
initialSkovDataDefault :: (IsProtocolVersion pv, BS.BlockStateQuery m, bs ~ BlockState m) => GenesisData pv -> bs -> m (SkovData pv bs)
initialSkovDataDefault = initialSkovData defaultRuntimeParameters

-- |Create initial skov data based on a genesis block and its state.
initialSkovData :: (IsProtocolVersion pv, BS.BlockStateQuery m, bs ~ BlockState m) => RuntimeParameters -> GenesisData pv -> bs -> m (SkovData pv bs)
initialSkovData rp gd genState = do
    acctAddrs <- BS.getAccountList genState
    acctNonces <- foldM (\nnces addr ->
        BS.getAccount genState addr >>= \case
            Nothing -> error "Invariant violation: listed account does not exist"
            Just (_, acct) -> do
                nonce <- BS.getAccountNonce acct
                return $! (addr,nonce):nnces) [] acctAddrs
    updSeqNums <- foldM (\m uty -> do
        sn <- BS.getNextUpdateSequenceNumber genState uty
        return $! Map.insert uty sn m) Map.empty [minBound..]
    return $ SkovData {
            _blockTable = HM.singleton gbh (TS.BlockFinalized gb gbfin),
            _finalizedByHeightTable = HM.singleton 0 gb,
            _possiblyPendingTable = HM.empty,
            _possiblyPendingQueue = MPQ.empty,
            _finalizationList = Seq.singleton (gbfin, gb),
            _branches = Seq.empty,
            _genesisData = gd,
            _genesisBlockPointer = gb,
            _focusBlock = gb,
            _pendingTransactions = emptyPendingTransactionTable,
            _transactionTable = emptyTransactionTableWithSequenceNumbers acctNonces updSeqNums,
            _statistics = initialConsensusStatistics,
            _runtimeParameters = rp,
            _transactionTablePurgeCounter = 0
        }
  where gbh = bpHash gb
        gbfin = FinalizationRecord 0 gbh emptyFinalizationProof 0
        gb = makeGenesisBasicBlockPointer gd genState

-- |Newtype wrapper that provides an implementation of the TreeStateMonad using a non-persistent tree state.
-- The underlying Monad must provide instances for:
--
-- * `BlockStateTypes`
-- * `BlockStateQuery`
-- * `BlockStateOperations`
-- * `BlockStateStorage`
-- * `MonadState (SkovData bs)`
--
-- This newtype establishes types for the @GlobalStateTypes@. The type variable @bs@ stands for the BlockState
-- type used in the implementation.
newtype PureTreeStateMonad (pv :: ProtocolVersion) bs m a = PureTreeStateMonad { runPureTreeStateMonad :: m a }
  deriving (Functor, Applicative, Monad, MonadIO, BlockStateTypes,
            BS.BlockStateQuery, BS.AccountOperations, BS.BlockStateOperations, BS.BlockStateStorage)

deriving instance (Monad m, MonadState (SkovData pv bs) m) => MonadState (SkovData pv bs) (PureTreeStateMonad pv bs m)

instance (bs ~ BlockState m) => GlobalStateTypes (PureTreeStateMonad pv bs m) where
    type BlockPointerType (PureTreeStateMonad pv bs m) = BasicBlockPointer pv bs

instance (bs ~ BlockState m, Monad m, MonadState (SkovData pv bs) m, IsProtocolVersion pv) => BlockPointerMonad (PureTreeStateMonad pv bs m) where
    blockState = return . _bpState
    bpParent = return . runIdentity . _bpParent
    bpLastFinalized = return . runIdentity . _bpLastFinalized

instance ATITypes (PureTreeStateMonad pv bs m) where
  type ATIStorage (PureTreeStateMonad pv bs m) = ()

instance (Monad m) => PerAccountDBOperations (PureTreeStateMonad pv bs m) where
  -- default instance because ati = ()

instance (bs ~ BlockState m, BS.BlockStateStorage m, Monad m, MonadIO m, MonadState (SkovData pv bs) m, IsProtocolVersion pv)
          => TS.TreeStateMonad pv (PureTreeStateMonad pv bs m) where
    makePendingBlock key slot parent bid pf n lastFin trs statehash transactionOutcomesHash time = do
        return $ makePendingBlock (signBlock key slot parent bid pf n lastFin trs statehash transactionOutcomesHash) time
    getBlockStatus bh = use (blockTable . at' bh)
    makeLiveBlock block parent lastFin st () arrTime energy = do
            let blockP = makeBasicBlockPointer block parent lastFin st arrTime energy
            blockTable . at' (getHash block) ?= TS.BlockAlive blockP
            return blockP
    markDead bh = blockTable . at' bh ?= TS.BlockDead
    markFinalized bh fr = use (blockTable . at' bh) >>= \case
            Just (TS.BlockAlive bp) -> do
              blockTable . at' bh ?= TS.BlockFinalized bp fr
              finalizedByHeightTable . at (bpHeight bp) ?= bp
            _ -> return ()
    markPending pb = blockTable . at' (getHash pb) ?= TS.BlockPending pb
    markAllNonFinalizedDead = blockTable %= fmap nonFinDead
        where
            nonFinDead TS.BlockPending{} = TS.BlockDead
            nonFinDead TS.BlockAlive{} = TS.BlockDead
            nonFinDead o = o
    getGenesisBlockPointer = use genesisBlockPointer
    getGenesisData = use genesisData
    getLastFinalized = use finalizationList >>= \case
            _ Seq.:|> (finRec,lf) -> return (lf, finRec)
            _ -> error "empty finalization list"
    getNextFinalizationIndex = FinalizationIndex . fromIntegral . Seq.length <$> use finalizationList
    addFinalization newFinBlock finRec = finalizationList %= (Seq.:|> (finRec, newFinBlock))
    getFinalizedAtIndex finIndex = fmap snd . Seq.lookup (fromIntegral finIndex) <$> use finalizationList
    getRecordAtIndex finIndex = fmap fst . Seq.lookup (fromIntegral finIndex) <$> use finalizationList
    getFinalizedAtHeight bHeight = preuse (finalizedByHeightTable . ix bHeight)
    getBranches = use branches
    putBranches brs = branches .= brs
    takePendingChildren bh = possiblyPendingTable . at' bh . non [] <<.= []
    addPendingBlock pb = do
        let parent = blockPointer (bbFields (pbBlock pb))
        possiblyPendingTable . at' parent . non [] %= (pb:)
        possiblyPendingQueue %= MPQ.insert (blockSlot (pbBlock pb)) (getHash pb, parent)
    takeNextPendingUntil slot = tnpu =<< use possiblyPendingQueue
        where
            tnpu ppq = case MPQ.minViewWithKey ppq of
                Just ((sl, (pbh, parenth)), ppq') ->
                    if sl <= slot then do
                        (myPB, otherPBs) <- partition ((== pbh) . pbHash) <$> use (possiblyPendingTable . at' parenth . non [])
                        case myPB of
                            [] -> tnpu ppq'
                            (realPB : _) -> do
                                possiblyPendingTable . at' parenth . non [] .= otherPBs
                                possiblyPendingQueue .= ppq'
                                return (Just realPB)
                    else do
                        possiblyPendingQueue .= ppq
                        return Nothing
                Nothing -> do
                    possiblyPendingQueue .= ppq
                    return Nothing
    wipePendingBlocks = do
        possiblyPendingTable .= HM.empty
        possiblyPendingQueue .= MPQ.empty
    getFocusBlock = use focusBlock
    putFocusBlock bb = focusBlock .= bb
    getPendingTransactions = use pendingTransactions
    putPendingTransactions pts = pendingTransactions .= pts
    getAccountNonFinalized addr nnce =
            use (transactionTable . ttNonFinalizedTransactions . at' addr) >>= \case
                Nothing -> return []
                Just anfts ->
                    let (_, atnnce, beyond) = Map.splitLookup nnce (anfts ^. anftMap)
                    in return $ case atnnce of
                        Nothing -> Map.toAscList beyond
                        Just s -> (nnce, s) : Map.toAscList beyond

    getNextAccountNonce addr =
        use (transactionTable . ttNonFinalizedTransactions . at' addr) >>= \case
                Nothing -> return (minNonce, True)
                Just anfts ->
                  case Map.lookupMax (anfts ^. anftMap) of
                    Nothing -> return (anfts ^. anftNextNonce, True) -- all transactions are finalized
                    Just (nonce, _) -> return (nonce + 1, False)

    getCredential txHash =
      preuse (transactionTable . ttHashMap . ix txHash) >>= \case
        Just (WithMetadata{wmdData=CredentialDeployment{..},..}, _) -> return $ Just WithMetadata{wmdData=biCred,..}
        _ -> return Nothing
    
    getNonFinalizedChainUpdates uty sn =
      use (transactionTable . ttNonFinalizedChainUpdates . at' uty) >>= \case
        Nothing -> return []
        Just nfcus ->
          let (_, atsn, beyond) = Map.splitLookup sn (nfcus ^. nfcuMap)
          in return $ case atsn of
            Nothing -> Map.toAscList beyond
            Just s -> (sn, s) : Map.toAscList beyond

    addCommitTransaction bi@WithMetadata{..} slot = do
      let trHash = wmdHash
      tt <- use transactionTable
      case tt ^. ttHashMap . at' trHash of
          Nothing ->
            case wmdData of
              NormalTransaction tr -> do
                let sender = accountAddressEmbed (transactionSender tr)
                    nonce = transactionNonce tr
                if (tt ^. ttNonFinalizedTransactions . at' sender . non emptyANFT . anftNextNonce) <= nonce then do
                  transactionTablePurgeCounter += 1
                  let wmdtr = WithMetadata{wmdData=tr,..}
                  transactionTable .= (tt & (ttNonFinalizedTransactions . at' sender . non emptyANFT . anftMap . at' nonce . non Set.empty %~ Set.insert wmdtr)
                                          & (ttHashMap . at' trHash ?~ (bi, Received slot)))
                  return (TS.Added bi)
                else return TS.ObsoleteNonce
              CredentialDeployment{} -> do
                transactionTable . ttHashMap . at' trHash ?= (bi, Received slot)
                return (TS.Added bi)
              ChainUpdate cu -> do
                let uty = updateType (uiPayload cu)
                    sn = updateSeqNumber (uiHeader cu)
                if (tt ^. ttNonFinalizedChainUpdates . at' uty . non emptyNFCU . nfcuNextSequenceNumber) <= sn then do
                  transactionTablePurgeCounter += 1
                  let wmdcu = WithMetadata{wmdData=cu,..}
                  transactionTable .= (tt
                          & (ttNonFinalizedChainUpdates . at' uty . non emptyNFCU . nfcuMap . at' sn . non Set.empty %~ Set.insert wmdcu)
                          & (ttHashMap . at' trHash ?~ (bi, Received slot)))
                  return (TS.Added bi)
                else return TS.ObsoleteNonce
          Just (_, Finalized{}) ->
            return TS.ObsoleteNonce
          Just (tr', results) -> do
            when (slot > results ^. tsSlot) $ transactionTable . ttHashMap . at' trHash . mapped . _2 %= updateSlot slot
            return $ TS.Duplicate tr'

    finalizeTransactions bh slot = mapM_ finTrans
        where
            finTrans WithMetadata{wmdData=NormalTransaction tr,..} = do
                let nonce = transactionNonce tr
                    sender = accountAddressEmbed (transactionSender tr)
                anft <- use (transactionTable . ttNonFinalizedTransactions . at' sender . non emptyANFT)
                assert (anft ^. anftNextNonce == nonce) $ do
                    let nfn = anft ^. anftMap . at' nonce . non Set.empty
                    let wmdtr = WithMetadata{wmdData=tr,..}
                    assert (Set.member wmdtr nfn) $ do
                        -- Remove any other transactions with this nonce from the transaction table.
                        -- They can never be part of any other block after this point.
                        forM_ (Set.delete wmdtr nfn) $
                          \deadTransaction -> transactionTable . ttHashMap . at' (getHash deadTransaction) .= Nothing
                        -- Mark the status of the transaction as finalized.
                        -- Singular here is safe due to the precondition (and assertion) that all transactions
                        -- which are part of live blocks are in the transaction table.
                        transactionTable . ttHashMap . singular (ix wmdHash) . _2 %=
                            \case Committed{..} -> Finalized{_tsSlot=slot,tsBlockHash=bh,tsFinResult=tsResults HM.! bh,..}
                                  _ -> error "Transaction should be in committed state when finalized."
                        -- Update the non-finalized transactions for the sender
                        transactionTable . ttNonFinalizedTransactions . at' sender ?= (anft & (anftMap . at' nonce .~ Nothing) & (anftNextNonce .~ nonce + 1))
            finTrans WithMetadata{wmdData=CredentialDeployment{},..} = do
              transactionTable . ttHashMap . singular (ix wmdHash) . _2 %=
                            \case Committed{..} -> Finalized{_tsSlot=slot,tsBlockHash=bh,tsFinResult=tsResults HM.! bh,..}
                                  _ -> error "Transaction should be in committed state when finalized."
            finTrans WithMetadata{wmdData=ChainUpdate cu,..} = do
                let sn = updateSeqNumber (uiHeader cu)
                    uty = updateType (uiPayload cu)
                nfcu <- use (transactionTable . ttNonFinalizedChainUpdates . at' uty . non emptyNFCU)
                assert (nfcu ^. nfcuNextSequenceNumber == sn) $ do
                    let nfsn = nfcu ^. nfcuMap . at' sn . non Set.empty
                    let wmdcu = WithMetadata{wmdData = cu,..}
                    assert (Set.member wmdcu nfsn) $ do
                        -- Remove any other updates with the same sequence number, since they weren't finalized
                        forM_ (Set.delete wmdcu nfsn) $ 
                          \deadUpdate -> transactionTable . ttHashMap . at' (getHash deadUpdate) .= Nothing
                        -- Mark this update as finalized.
                        transactionTable . ttHashMap . singular (ix wmdHash) . _2 %=
                            \case Committed{..} -> Finalized{_tsSlot=slot,tsBlockHash=bh,tsFinResult=tsResults HM.! bh,..}
                                  _ -> error "Transaction should be in committed state when finalized."
                        -- Update the non-finalized chain updates
                        transactionTable . ttNonFinalizedChainUpdates . at' uty ?=
                          (nfcu & (nfcuMap . at' sn .~ Nothing) & (nfcuNextSequenceNumber .~ sn + 1))

    commitTransaction slot bh tr idx =
        transactionTable . ttHashMap . at' (getHash tr) %= fmap (_2 %~ addResult bh slot idx)

    purgeTransaction WithMetadata{..} =
        use (transactionTable . ttHashMap . at' wmdHash) >>= \case
            Nothing -> return True
            Just (_, results) -> do
                lastFinSlot <- blockSlot . _bpBlock . fst <$> TS.getLastFinalized
                if lastFinSlot >= results ^. tsSlot then do
                    -- remove from the table
                    transactionTable . ttHashMap . at' wmdHash .= Nothing
                    -- if the transaction is from a sender also delete the relevant
                    -- entry in the account non finalized table
                    case wmdData of
                      NormalTransaction tr -> do
                        let nonce = transactionNonce tr
                            sender = accountAddressEmbed (transactionSender tr)
                        transactionTable
                          . ttNonFinalizedTransactions
                          . at' sender
                          . non emptyANFT
                          . anftMap
                          . at' nonce
                          . non Set.empty %= Set.delete WithMetadata{wmdData=tr,..}
                      _ -> return () -- do nothing.
                    return True
                else return False

    markDeadTransaction bh tr =
      -- We only need to update the outcomes. The anf table nor the pending table need be updated
      -- here since a transaction should not be marked dead in a finalized block.
      transactionTable . ttHashMap . at' (getHash tr) . mapped . _2 %= markDeadResult bh
    lookupTransaction th =
       preuse (transactionTable . ttHashMap . ix th . _2)

    getConsensusStatistics = use statistics
    putConsensusStatistics stats = statistics .= stats

    {-# INLINE getRuntimeParameters #-}
    getRuntimeParameters = use runtimeParameters

    purgeTransactionTable ignoreInsertions currentTime = do
      purgeCount <- use transactionTablePurgeCounter
      RuntimeParameters{..} <- use runtimeParameters
      when (ignoreInsertions || purgeCount > rpInsertionsBeforeTransactionPurge) $ do
        transactionTablePurgeCounter .= 0
        lastFinalizedSlot <- TS.getLastFinalizedSlot
        transactionTable' <- use transactionTable
        pendingTransactions' <- use pendingTransactions
        let
          currentTransactionTime = utcTimeToTransactionTime currentTime
          oldestArrivalTime = if currentTransactionTime > rpTransactionsKeepAliveTime
                                then currentTransactionTime - rpTransactionsKeepAliveTime
                                else 0
          currentTimestamp = utcTimeToTimestamp currentTime
          (newTT, newPT) = purgeTables lastFinalizedSlot oldestArrivalTime currentTimestamp transactionTable' pendingTransactions'
        transactionTable .= newTT
        pendingTransactions .= newPT

    wipeNonFinalizedTransactions = do
        let consNonFin (_, Finalized{}) = id
            consNonFin (bi, _) = (bi :)
            isFin (_, Finalized{}) = True
            isFin _ = False
        -- The motivation for using foldr here is that the list will be consumed by iteration
        -- almost immediately, so it is reasonable to build it lazily.
        oldTransactions <- HM.foldr consNonFin [] <$> use (transactionTable . ttHashMap)
        -- Filter to only the finalized transactions.
        transactionTable %= (ttHashMap %~ HM.filter isFin)
                . (ttNonFinalizedTransactions %~ fmap (anftMap .~ Map.empty))
                . (ttNonFinalizedChainUpdates %~ fmap (nfcuMap .~ Map.empty))
        return oldTransactions
