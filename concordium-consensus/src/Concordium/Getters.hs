{-# LANGUAGE
    OverloadedStrings,
    ScopedTypeVariables,
    CPP,
    MonoLocalBinds #-}
module Concordium.Getters where

import Lens.Micro.Platform hiding ((.=))

import Concordium.Kontrol.BestBlock
import Concordium.Skov

import Control.Monad.State.Class

import qualified Concordium.Scheduler.Types as AT
import Concordium.GlobalState.Classes
import qualified Concordium.GlobalState.TreeState as TS
import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.Statistics as Stat
import Concordium.Types as T
import Concordium.GlobalState.Information(jsonStorable)
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Bakers
import Concordium.GlobalState.Block
import Concordium.Types.HashableTo
import qualified Concordium.Types.Acorn.Core as Core
import Concordium.GlobalState.Instance
import Concordium.GlobalState.Finalization
import qualified Concordium.Skov.CatchUp as CU
import qualified Data.PQueue.Prio.Max as Queue

import Concordium.Afgjort.Finalize(FinalizationStateLenses(..))

import Control.Concurrent.MVar
import Data.IORef
import Text.Read hiding (get, String)
import qualified Data.Map as Map
import Data.Aeson
import qualified Data.Text as T
import Data.Word
import Data.Vector (fromList)

class SkovQueryMonad m => SkovStateQueryable z m | z -> m where
    runStateQuery :: z -> m a -> IO a

instance (SkovConfiguration c, SkovQueryMonad (SkovT () c IO))
        => SkovStateQueryable (SkovContext c, IORef (SkovState c)) (SkovT () c IO) where
    runStateQuery (ctx, st) a = readIORef st >>= evalSkovT a () ctx

instance (SkovConfiguration c, SkovQueryMonad (SkovT () c IO))
        => SkovStateQueryable (SkovContext c, MVar (SkovState c)) (SkovT () c IO) where
    runStateQuery (ctx, st) a = readMVar st >>= evalSkovT a () ctx

hsh :: (HashableTo BlockHash a) => a -> String
hsh x = show (getHash x :: BlockHash)

getBestBlockState :: SkovQueryMonad m => m (BlockState m)
getBestBlockState = queryBlockState =<< bestBlock

getLastFinalState :: SkovQueryMonad m => m (BlockState m)
getLastFinalState = queryBlockState =<< lastFinalizedBlock

withBlockStateJSON :: SkovQueryMonad m => BlockHash -> (BlockState m -> m Value) -> m Value
withBlockStateJSON hash f =
  resolveBlock hash >>=
    \case Nothing -> return Null
          Just bp -> f =<< queryBlockState bp

getAccountList :: SkovStateQueryable z m => BlockHash -> z -> IO Value
getAccountList hash sfsRef = runStateQuery sfsRef $
  withBlockStateJSON hash $ \st -> do
  alist <- BS.getAccountList st
  return . toJSON $ alist  -- show instance for account addresses is based on Base58 encoding

getInstances :: (SkovStateQueryable z m) => BlockHash -> z -> IO Value
getInstances hash sfsRef = runStateQuery sfsRef $
  withBlockStateJSON hash $ \st -> do
  ilist <- BS.getContractInstanceList st
  return $ toJSON (map iaddress ilist)

getAccountInfo :: (SkovStateQueryable z m) => BlockHash -> z -> AccountAddress -> IO Value
getAccountInfo hash sfsRef addr = runStateQuery sfsRef $
  withBlockStateJSON hash $ \st ->
  BS.getAccount st addr >>=
      \case Nothing -> return Null
            Just acc -> return $ object ["accountNonce" .= let Nonce n = (acc ^. T.accountNonce) in n
                                        ,"accountAmount" .= toInteger (acc ^. T.accountAmount)
                                        ,"accountCredentials" .= Queue.toList (acc ^. accountCredentials)
                                        ,"accountDelegation" .= (toInteger <$> (acc ^. T.accountStakeDelegate))
                                        ]

getContractInfo :: (SkovStateQueryable z m) => BlockHash -> z -> AT.ContractAddress -> IO Value
getContractInfo hash sfsRef addr = runStateQuery sfsRef $
  withBlockStateJSON hash $ \st ->
  BS.getContractInstance st addr >>=
      \case Nothing -> return Null
            Just istance -> let params = instanceParameters istance
                            in return $ object ["model" .= jsonStorable (instanceModel istance)
                                               ,"owner" .= String (T.pack (show (instanceOwner params))) -- account address show instance is base58
                                               ,"amount" .= toInteger (instanceAmount istance)]

getRewardStatus :: (SkovStateQueryable z m) => BlockHash -> z -> IO Value
getRewardStatus hash sfsRef = runStateQuery sfsRef $
  withBlockStateJSON hash $ \st -> do
  reward <- BS.getRewardStatus st
  return $ object [
    "totalAmount" .= (fromIntegral (reward ^. AT.totalGTU) :: Integer),
    "totalEncryptedAmount" .= (fromIntegral (reward ^. AT.totalEncryptedGTU) :: Integer),
    "centralBankAmount" .= (fromIntegral (reward ^. AT.centralBankGTU) :: Integer),
    "mintedAmountPerSlot" .= (fromIntegral (reward ^. AT.mintedGTUPerSlot) :: Integer)
    ]

getBlockBirkParameters :: (SkovStateQueryable z m) => BlockHash -> z -> IO Value
getBlockBirkParameters hash sfsRef = runStateQuery sfsRef $
  withBlockStateJSON hash $ \st -> do
  bps@BirkParameters{..} <- BS.getBlockBirkParameters st
  return $ object [
    "electionDifficulty" .= _birkElectionDifficulty,
    "electionNonce" .= _birkLeadershipElectionNonce bps,
    "bakers" .= Array (fromList .
                       map (\(bid, BakerInfo{..}) -> object ["bakerId" .= toInteger bid
                                                            ,"bakerAccount" .= show _bakerAccount
                                                            ,"bakerLotteryPower" .= ((fromIntegral _bakerStake :: Double) / fromIntegral (_bakerTotalStake _birkLotteryBakers))
                                                            ]) .
                       Map.toList $ _bakerMap _birkLotteryBakers )
    ]

getModuleList :: (SkovStateQueryable z m) => BlockHash -> z -> IO Value
getModuleList hash sfsRef = runStateQuery sfsRef $
  withBlockStateJSON hash $ \st -> do
  mlist <- BS.getModuleList st
  return . toJSON . map show $ mlist -- show instance of ModuleRef displays it in Base16


getModuleSource :: (SkovStateQueryable z m) => BlockHash -> z -> ModuleRef -> IO (Maybe (Core.Module Core.UA))
getModuleSource hash sfsRef mhash = runStateQuery sfsRef $
  resolveBlock hash >>=
    \case Nothing -> return Nothing
          Just bp -> do
            st <- queryBlockState bp
            mmodul <- BS.getModule st mhash
            return (BS.moduleSource <$> mmodul)

getConsensusStatus :: (SkovStateQueryable z m, TS.TreeStateMonad m) => z -> IO Value
getConsensusStatus sfsRef = runStateQuery sfsRef $ do
        bb <- bestBlock
        lfb <- lastFinalizedBlock
        genesis <- genesisBlock
        stats <- TS.getConsensusStatistics
        return $ object [
                "bestBlock" .= hsh bb,
                "genesisBlock" .= hsh genesis,
                "lastFinalizedBlock" .= hsh lfb,
                "bestBlockHeight" .= theBlockHeight (bpHeight bb),
                "lastFinalizedBlockHeight" .= theBlockHeight (bpHeight lfb),
                "blocksReceivedCount" .= (stats ^. Stat.blocksReceivedCount),
                "blockLastReceivedTime" .= (stats ^. Stat.blockLastReceived),
                "blockReceiveLatencyEMA" .= (stats ^. Stat.blockReceiveLatencyEMA),
                "blockReceiveLatencyEMSD" .= sqrt (stats ^. Stat.blockReceiveLatencyEMVar),
                "blockReceivePeriodEMA" .= (stats ^. Stat.blockReceivePeriodEMA),
                "blockReceivePeriodEMSD" .= (sqrt <$> (stats ^. Stat.blockReceivePeriodEMVar)),
                "blocksVerifiedCount" .= (stats ^. Stat.blocksVerifiedCount),
                "blockLastArrivedTime" .= (stats ^. Stat.blockLastArrive),
                "blockArriveLatencyEMA" .= (stats ^. Stat.blockArriveLatencyEMA),
                "blockArriveLatencyEMSD" .= sqrt (stats ^. Stat.blockArriveLatencyEMVar),
                "blockArrivePeriodEMA" .= (stats ^. Stat.blockArrivePeriodEMA),
                "blockArrivePeriodEMSD" .= (sqrt <$> (stats ^. Stat.blockArrivePeriodEMVar)),
                "transactionsPerBlockEMA" .= (stats ^. Stat.transactionsPerBlockEMA),
                "transactionsPerBlockEMSD" .= sqrt (stats ^. Stat.transactionsPerBlockEMVar),
                "finalizationCount" .= (stats ^. Stat.finalizationCount),
                "lastFinalizedTime" .= (stats ^. Stat.lastFinalizedTime),
                "finalizationPeriodEMA" .= (stats ^. Stat.finalizationPeriodEMA),
                "finalizationPeriodEMSD" .= (sqrt <$> (stats ^. Stat.finalizationPeriodEMVar))
            ]

getBlockInfo :: (SkovStateQueryable z m, TS.TreeStateMonad m) => z -> String -> IO Value
getBlockInfo sfsRef blockHash = case readMaybe blockHash of
        Nothing -> return Null
        Just bh -> runStateQuery sfsRef $
                resolveBlock bh >>= \case
                    Nothing -> return Null
                    Just bp -> do
                        let slot = blockSlot bp
                        st <- queryBlockState bp
                        reward <- BS.getRewardStatus st
                        slotTime <- getSlotTime slot
                        bfin <- isFinalized bh
                        return $ object [
                            "blockHash" .= hsh bp,
                            "blockParent" .= hsh (bpParent bp),
                            "blockLastFinalized" .= hsh (bpLastFinalized bp),
                            "blockHeight" .= theBlockHeight (bpHeight bp),
                            "blockReceiveTime" .= bpReceiveTime bp,
                            "blockArriveTime" .= bpArriveTime bp,
                            "blockSlot" .= (fromIntegral slot :: Word64),
                            "blockSlotTime" .= slotTime,
                            "blockBaker" .= case blockFields bp of
                                            Nothing -> Null
                                            Just bf -> toJSON (toInteger (blockBaker bf)),
                            "finalized" .= bfin,
                            "transactionCount" .= bpTransactionCount bp,
                            "transactionEnergyCost" .= toInteger (bpTransactionsEnergyCost bp),
                            "transactionsSize" .= toInteger (bpTransactionsSize bp),

                            "totalAmount" .= toInteger (reward ^. AT.totalGTU),
                            "totalEncryptedAmount" .= toInteger (reward ^. AT.totalEncryptedGTU),
                            "centralBankAmount" .= toInteger (reward ^. AT.centralBankGTU),
                            "mintedAmountPerSlot" .= toInteger (reward ^. AT.mintedGTUPerSlot),
                            "executionCost" .= toInteger (reward ^. AT.executionCost)
                            ]

getAncestors :: (SkovStateQueryable z m, TS.TreeStateMonad m) => z -> String -> BlockHeight -> IO Value
getAncestors sfsRef blockHash count = case readMaybe blockHash of
        Nothing -> return Null
        Just bh -> runStateQuery sfsRef $
                resolveBlock bh >>= \case
                    Nothing -> return Null
                    Just bp ->
                        return $ toJSONList $ map hsh $ take (fromIntegral $ min count (1 + bpHeight bp)) $ iterate bpParent bp

getBranches :: (SkovStateQueryable z m, TS.TreeStateMonad m) => z -> IO Value
getBranches sfsRef = runStateQuery sfsRef $ do
            brs <- branchesFromTop
            let brt = foldl up Map.empty brs
            lastFin <- lastFinalizedBlock
            return $ object ["blockHash" .= hsh lastFin, "children" .= Map.findWithDefault [] lastFin brt]
    where
        up childrenMap = foldr (\b -> at (bpParent b) . non [] %~ (object ["blockHash" .= hsh b, "children" .= Map.findWithDefault [] b childrenMap] :)) Map.empty

getBlockFinalization :: (SkovStateQueryable z m, TS.TreeStateMonad m) => z -> BlockHash -> IO (Maybe FinalizationRecord)
getBlockFinalization sfsRef bh = runStateQuery sfsRef $ do
            bs <- TS.getBlockStatus bh
            case bs of
                Just (TS.BlockFinalized _ fr) -> return $ Just fr
                _ -> return Nothing

-- |Check whether a keypair is part of the baking committee by a key pair in the current best block.
-- Returns 0 if keypair is not added as a baker.
-- Returns 1 if keypair is added as a baker, but not part of the baking committee yet.
-- Returns 2 if keypair is part of the baking committee.
checkBakerExistsBestBlock :: (SkovStateQueryable z m)
    => BakerSignVerifyKey
    -> z
    -> IO Word8
checkBakerExistsBestBlock key sfsRef = runStateQuery sfsRef $ do
  bb <- bestBlock
  bps <- BS.getBlockBirkParameters =<< queryBlockState bb
  case bps ^. birkLotteryBakers . bakersByKey . at key of
    Just _ -> return 2
    Nothing -> 
      case bps ^. birkCurrentBakers . bakersByKey . at key of
        Just _ -> return 1
        Nothing -> return 0

-- |Check whether a keypair is part of the finalization committee by a key pair in the current best block.
-- checkFinalizerExistsBestBlock :: (SkovStateQueryable z m, FinalizationMonad s m) => z -> IO Bool
checkFinalizerExistsBestBlock :: (SkovStateQueryable z m, MonadState s m, FinalizationStateLenses s t) => z -> IO Bool
checkFinalizerExistsBestBlock sfsRef = runStateQuery sfsRef $ do
   fs <- use finState
   case fs ^. finCurrentRound of
     Nothing -> return False
     Just _ -> return True

getCatchUpStatus :: (SkovStateQueryable z m, TS.TreeStateMonad m) => z -> IO CU.CatchUpStatus
getCatchUpStatus sRef = runStateQuery sRef $ CU.getCatchUpStatus True

handleCatchUpStatus :: (SkovStateQueryable z m, TS.TreeStateMonad m) => z -> CU.CatchUpStatus -> IO (Either String (Maybe ([Either FinalizationRecord (BlockPointer m)], CU.CatchUpStatus), Bool))
handleCatchUpStatus sRef cus = runStateQuery sRef $ CU.handleCatchUp cus
