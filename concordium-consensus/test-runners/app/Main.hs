{-# LANGUAGE
    BangPatterns,
    OverloadedStrings,
    CPP,
    ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Main where

import Control.Concurrent
import Control.Monad
import System.Random
import Data.Time.Clock.POSIX
import System.IO
import Data.Serialize
import Control.Exception
import System.Directory

import Concordium.TimerMonad
import Concordium.Types.HashableTo
import Concordium.GlobalState.IdentityProviders
import Concordium.GlobalState.Parameters
import Concordium.Types.Transactions
import Concordium.GlobalState.Block
import Concordium.GlobalState.Paired
import Concordium.GlobalState.BlockPointer
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Instance
import qualified Concordium.GlobalState.Basic.BlockState as Basic
import Concordium.GlobalState.BlockState
import Concordium.GlobalState
import Concordium.Kontrol (currentTimestamp)

import Concordium.Logger
import Concordium.Types
import Concordium.Runner
import Concordium.Skov
import Concordium.Getters
import Concordium.Afgjort.Finalize (FinalizationInstance(..))
import Concordium.Birk.Bake

import Concordium.Scheduler.Utils.Init.Example as Example
--import Debug.Trace
import Concordium.Startup
import Concordium.Crypto.DummyData (mateuszKP)

nContracts :: Int
nContracts = 2

transactions :: StdGen -> [BlockItem]
transactions gen = trs (0 :: Nonce) (randoms gen :: [Int])
    where
        --contr i = ContractAddress (fromIntegral $ i `mod` nContracts) 0
        trs n (_ : _ : rs) = Example.makeTransferTransaction (mateuszKP, mateuszAccount) mateuszAccount 123 n : trs (n+1) rs
        trs _ _ = error "Ran out of transaction data"

sendTransactions :: Int -> Chan (InMessage a) -> [BlockItem] -> IO ()
sendTransactions bakerId chan (t : ts) = do
        (writeChan chan (MsgTransactionReceived $ encode $ wmdData t))
        -- r <- randomRIO (5000, 15000)
        threadDelay 200000
        sendTransactions bakerId chan ts
sendTransactions _ _ _ = return ()

relay :: Chan (OutMessage src) -> SyncRunner ActiveConfig -> Chan (Either (BlockHash, BakedBlock, (Maybe BlockHeight, [Instance])) FinalizationRecord) -> [Chan (InMessage ())] -> IO ()
relay inp sr monitor outps = loop `catch` (\(e :: SomeException) -> hPutStrLn stderr $ "// *** relay thread exited on exception: " ++ show e)
    where
        loop = do
            msg <- readChan inp
            now <- getTransactionTime
            case msg of
                MsgNewBlock blockBS -> do
                    case runGet (getBlock now) blockBS of
                        Right (NormalBlock !block) -> do
                            let bh = getHash block :: BlockHash
                            bi <- runStateQuery sr (bInsts bh)
                            writeChan monitor (Left (bh, block, bi))
                        _ -> return ()
                    forM_ outps $ \outp -> forkIO $ do
                        --factor <- (/2) . (+1) . sin . (*(pi/240)) . fromRational . toRational <$> getPOSIXTime
                        let factor = 0.05 :: Double
                        r <- truncate . (*factor) . fromInteger . (`div` 10) . (^(2::Int)) <$> randomRIO (0, 7800)
                        threadDelay r
                        --putStrLn $ "Delay: " ++ show r
                        writeChan outp (MsgBlockReceived () blockBS)
                MsgFinalization bs ->
                    forM_ outps $ \outp -> forkIO $ do
                        -- factor <- (/2) . (+1) . sin . (*(pi/240)) . fromRational . toRational <$> getPOSIXTime
                        let factor = 0.05 :: Double
                        r <- truncate . (*factor) . fromInteger . (`div` 10) . (^(2::Int)) <$> randomRIO (0, 7800)
                        threadDelay r
                        --putStrLn $ "Delay: " ++ show r
                        writeChan outp (MsgFinalizationReceived () bs)
                MsgFinalizationRecord fr -> do
                    case runGet get fr of
                        Right !fr' -> writeChan monitor (Right fr')
                        _ -> return ()
                    forM_ outps $ \outp -> forkIO $ do
                        -- factor <- (/2) . (+1) . sin . (*(pi/240)) . fromRational . toRational <$> getPOSIXTime
                        let factor = 0.05 :: Double
                        r <- truncate . (*factor) . fromInteger . (`div` 10) . (^(2::Int)) <$> randomRIO (0, 7800)
                        threadDelay r
                        --putStrLn $ "Delay: " ++ show r
                        writeChan outp (MsgFinalizationRecordReceived () fr)
                _ -> return ()
            loop
        bInsts :: BlockHash -> SkovT () ActiveConfig LogIO (Maybe BlockHeight, [Instance])
        bInsts bh = do
            bst <- resolveBlock bh
            case bst of
                Nothing -> return (Nothing, [])
                Just bs -> fmap (Just (bpHeight bs), ) . getContractInstanceList =<< queryBlockState bs

removeEach :: [a] -> [(a,[a])]
removeEach = re []
    where
        re l (x:xs) = (x,l++xs) : re (x:l) xs
        re _ [] = []

{-
gsToString :: BlockState -> String
gsToString gs = (show (currentSeed (gs ^.  blockBirkParameters ^. birkSeedState))) ++
                    "\n current: " ++ showBakers ( (gs ^. blockBirkParameters ^. birkCurrentBakers)) ++
                    "\n prev   : " ++ showBakers ( (gs ^. blockBirkParameters ^. birkPrevEpochBakers)) ++
                    "\n lottery: " ++ showBakers ( (gs ^. blockBirkParameters ^. birkLotteryBakers))
    where
        ca n = ContractAddress (fromIntegral n) 0
        keys = map (\n -> (n, instanceModel <$> getInstance (ca n) (gs ^. blockInstances))) $ enumFromTo 0 (nContracts-1)
        showBakers bs = show [ _bakerStake binfo | (_, binfo) <- Map.toList (_bakerMap bs)]
-}

dummyIdentityProviders :: [IpInfo]
dummyIdentityProviders = []

genesisState :: GenesisData -> Basic.BlockState
genesisState GenesisData{..} = Example.initialState
                       (Basic.BasicBirkParameters genesisElectionDifficulty genesisBakers genesisBakers genesisBakers genesisSeedState)
                       genesisCryptographicParameters
                       genesisAccounts
                       genesisIdentityProviders
                       2
                       genesisControlAccounts

type TreeConfig = DiskTreeDiskBlockConfig
makeGlobalStateConfig :: RuntimeParameters -> GenesisData -> IO TreeConfig
makeGlobalStateConfig rt genData = return $ DTDBConfig rt genData (genesisState genData)

-- type TreeConfig = MemoryTreeDiskBlockConfig
-- makeGlobalStateConfig :: RuntimeParameters -> GenesisData -> IO TreeConfig
-- makeGlobalStateConfig rt genData = return $ MTDBConfig rt genData (genesisState genData)

-- type TreeConfig = MemoryTreeMemoryBlockConfig
-- makeGlobalStateConfig :: RuntimeParameters -> GenesisData -> IO TreeConfig
-- makeGlobalStateConfig rt genData = return $ MTMBConfig rt genData (genesisState genData)

-- uncomment if wanting paired config
-- type TreeConfig = PairGSConfig MemoryTreeMemoryBlockConfig DiskTreeDiskBlockConfig

-- makeGlobalStateConfig :: RuntimeParameters -> GenesisData -> IO TreeConfig
-- makeGlobalStateConfig rp genData =
--    return $ PairGSConfig (MTMBConfig rp genData (genesisState genData), DTDBConfig rp genData (genesisState genData))

type ActiveConfig = SkovConfig TreeConfig (BufferedFinalization ThreadTimer) NoHandler

main :: IO ()
main = do
    let n = 5
    now <- currentTimestamp -- return 1588916588000
    let (gen, bis) = makeGenesisData now n 200 0.2
                     defaultFinalizationParameters{
                         finalizationCommitteeMaxSize = 3 * fromIntegral n + 1,
                         -- finalizationOldStyleSkip = True, finalizationSkipShrinkFactor = 0.5,
                         finalizationOldStyleSkip = False, finalizationSkipShrinkFactor = 0.8, finalizationSkipGrowFactor = 1.25,
                         finalizationAllowZeroDelay = True,
                         finalizationIgnoreFirstWait = True
                     }
                     dummyCryptographicParameters
                     dummyIdentityProviders
                     [createCustomAccount 1000000000000 mateuszKP mateuszAccount] (Energy maxBound)
    trans <- transactions <$> newStdGen
    createDirectoryIfMissing True "data"
    chans <- mapM (\(bakerId, (bid, _)) -> do
        logFile <- openFile ("consensus-" ++ show now ++ "-" ++ show bakerId ++ ".log") WriteMode

        let logM src lvl msg = {- when (lvl == LLInfo) $ -} do
                                    timestamp <- getCurrentTime
                                    hPutStrLn logFile $ "[" ++ show timestamp ++ "] " ++ show lvl ++ " - " ++ show src ++ ": " ++ msg
                                    hFlush logFile
        --let dbConnString = "host=localhost port=5432 user=txlog dbname=baker_" <> BS8.pack (show (1 + bakerId)) <> " password=txlogpassword"
        gsconfig <- makeGlobalStateConfig (defaultRuntimeParameters { rpTreeStateDir = "data/treestate-" ++ show now ++ "-" ++ show bakerId, rpBlockStateFile = "data/blockstate-" ++ show now ++ "-" ++ show bakerId }) gen --dbConnString
        let
            finconfig = BufferedFinalization (FinalizationInstance (bakerSignKey bid) (bakerElectionKey bid) (bakerAggregationKey bid))
            hconfig = NoHandler
            config = SkovConfig gsconfig finconfig hconfig
        (cin, cout, sr) <- makeAsyncRunner logM bid config
        _ <- forkIO $ sendTransactions bakerId cin trans
        return (cin, cout, sr)) (zip [(0::Int) ..] bis)
    monitorChan <- newChan
    mapM_ (\((_,cout, sr), cs) -> forkIO $ relay cout sr monitorChan ((\(c, _, _) -> c) <$> cs)) (removeEach chans)
    let loop =
            readChan monitorChan >>= \case
                Left (bh, block, gs') -> do
                    let ts = blockTransactions block
                    -- let stateStr = show gs'
                    let finInfo = case bfBlockFinalizationData (bbFields block) of
                            NoFinalizationData -> ""
                            BlockFinalizationData fr -> "\\lfin.wits:" ++ show (finalizationProofParties $ finalizationProof fr)
                    putStrLn $ " n" ++ show bh ++ " [label=\"" ++ show (blockBaker block) ++ ": " ++ show (blockSlot block) ++ " [" ++ show (length ts) ++ "]" ++ finInfo ++ show (fst gs') ++ "\"];"
                    putStrLn $ " n" ++ show bh ++ " -> n" ++ show (blockPointer block) ++ ";"
                    case (blockFinalizationData block) of
                        NoFinalizationData -> return ()
                        BlockFinalizationData fr ->
                            putStrLn $ " n" ++ show bh ++ " -> n" ++ show (finalizationBlockPointer fr) ++ " [style=dotted];"
                    hFlush stdout
                    loop
                Right fr -> do
                    putStrLn $ " n" ++ show (finalizationBlockPointer fr) ++ " [color=green];"
                    loop
    loop
