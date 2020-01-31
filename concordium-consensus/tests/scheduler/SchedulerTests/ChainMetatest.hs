{-# LANGUAGE OverloadedStrings #-}
module SchedulerTests.ChainMetatest where

import Test.Hspec
import Test.HUnit

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import qualified Acorn.Utils.Init as Init
import Concordium.Scheduler.Runner
import qualified Acorn.Parser.Runner as PR
import qualified Concordium.Scheduler as Sch
import qualified Acorn.Core as Core

import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Invariants
import Concordium.GlobalState.Basic.BlockState.Instances as Ins
import Concordium.GlobalState.Basic.BlockState.Account as Acc
import Concordium.GlobalState.Modules as Mod
import Concordium.GlobalState.Rewards as Rew

import Lens.Micro.Platform

import qualified Data.Text.IO as TIO
import qualified Data.Sequence as Seq

import Control.Monad.IO.Class

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

shouldReturnP :: Show a => IO a -> (a -> Bool) -> IO ()
shouldReturnP action f = action >>= (`shouldSatisfy` f)

initialBlockState :: BlockState
initialBlockState =
  emptyBlockState emptyBirkParameters dummyCryptographicParameters &
    (blockAccounts .~ Acc.putAccountWithRegIds (mkAccount alesVK alesAccount 100000) Acc.emptyAccounts) .
    (blockModules .~ (let (_, _, gs) = Init.baseState in Mod.fromModuleList (Init.moduleList gs))) .
    (blockBank . Rew.totalGTU .~ 100000)

chainMeta :: Types.ChainMetadata
chainMeta = Types.ChainMetadata{..}
  where slotNumber = 8
        blockHeight = 13
        finalizedHeight = 10
        slotTime = dummySlotTime

transactionsInput :: [TransactionJSON]
transactionsInput =
    [TJSON { payload = DeployModule "ChainMetaTest"
           , metadata = makeDummyHeader alesAccount 1 1000
           , keypair = alesKP
           }
    ,TJSON { payload = InitContract {amount = 123
                                    ,contractName = "Simple"
                                    ,moduleName = "ChainMetaTest"
                                    ,parameter = "Unit.Unit"
                                    }
           , metadata = makeDummyHeader alesAccount 2 10000
           , keypair = alesKP
           }
    ]


testChainMeta ::
  PR.Context Core.UA
    IO
    ([(Types.BareTransaction, Types.ValidResult)],
     [(Types.BareTransaction, Types.FailureKind)],
     [(Types.ContractAddress, Instance)])
testChainMeta = do
    source <- liftIO $ TIO.readFile "test/contracts/ChainMetaTest.acorn"
    (_, _) <- PR.processModule source -- execute only for effect on global state, i.e., load into cache
    transactions <- processTransactions transactionsInput
    let ((Sch.FilteredTransactions{..}, _), gs) =
          Types.runSI (Sch.filterTransactions dummyBlockSize (Types.Energy maxBound) transactions)
            dummySpecialBetaAccounts
            chainMeta
            initialBlockState
    case invariantBlockState gs of
        Left f -> liftIO $ assertFailure $ f ++ " " ++ show gs
        _ -> return ()
    return (ftAdded, ftFailed, gs ^.. blockInstances . foldInstances . to (\i -> (iaddress i, i)))

checkChainMetaResult :: ([(a1, Types.ValidResult)], [b], [(a3, Instance)]) -> Bool
checkChainMetaResult (suc, fails, instances) =
  null fails && -- should be no failed transactions
  null reject && -- no rejected transactions either
  length instances == 1 && -- only a single contract instance should be created
  checkLocalState (snd (head instances)) -- and the local state should match the
  where
    reject = filter (\case (_, Types.TxSuccess{}) -> False
                           (_, Types.TxReject{}) -> True
                    )
                        suc
    checkLocalState inst =
      case Types.instanceModel inst of
        Types.VConstructor _ (Types.VLiteral (Core.Word64 8) Seq.:<|  -- NB: These should match those in chainMeta
                              Types.VLiteral (Core.Word64 13) Seq.:<|
                              Types.VLiteral (Core.Word64 10) Seq.:<| Seq.Empty) -> True
        _ -> False

tests :: SpecWith ()
tests =
  describe "Chain metadata in transactions." $
    specify "Reading chain metadata." $
      PR.evalContext Init.initialContextData testChainMeta `shouldReturnP` checkChainMetaResult
