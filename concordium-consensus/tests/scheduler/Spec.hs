module Main where

import qualified SchedulerTests.InitPoliciesTest(tests)
import qualified SchedulerTests.SimpleTransfersTest(tests)
import qualified SchedulerTests.ChainMetatest(tests)
import qualified SchedulerTests.InitContextTest(tests)
import qualified SchedulerTests.ReceiveContextTest(tests)
import qualified SchedulerTests.TrySendTest(tests)
import qualified SchedulerTests.FibonacciSelfMessageTest(tests)
import qualified SchedulerTests.SmartContractTests(tests)
import qualified SchedulerTests.AccountTransactionSpecs(tests)
import qualified SchedulerTests.InitialAccountCreationSpec(tests)
import qualified SchedulerTests.BakerTransactions(tests)
import qualified SchedulerTests.RandomBakerTransactions(tests)
import qualified SchedulerTests.TransactionExpirySpec(tests)
import qualified SchedulerTests.BlockEnergyLimitSpec(tests)
import qualified SchedulerTests.TransactionGroupingSpec2(tests)
import qualified SchedulerTests.SimpleTransferSpec(tests)
import qualified SchedulerTests.UpdateAccountKeys(tests)
import qualified SchedulerTests.UpdateCredentials(tests)
import qualified SchedulerTests.TransfersWithScheduleTest(tests)
import qualified SchedulerTests.EncryptedTransfersTest(tests)
import qualified SchedulerTests.MaxIncomingAmountsTest(tests)
import qualified SchedulerTests.StakedAmountLocked(tests)
import qualified SchedulerTests.RejectReasons(tests)
import qualified SchedulerTests.RejectReasonsRustContract(tests)

import Test.Hspec

main :: IO ()
main = hspec $ do
         SchedulerTests.InitPoliciesTest.tests
         SchedulerTests.SimpleTransfersTest.tests
         SchedulerTests.ChainMetatest.tests
         SchedulerTests.InitContextTest.tests
         SchedulerTests.ReceiveContextTest.tests
         SchedulerTests.TrySendTest.tests
         SchedulerTests.FibonacciSelfMessageTest.tests
         SchedulerTests.SmartContractTests.tests
         SchedulerTests.AccountTransactionSpecs.tests
         SchedulerTests.InitialAccountCreationSpec.tests
         SchedulerTests.BakerTransactions.tests
         SchedulerTests.RandomBakerTransactions.tests
         SchedulerTests.TransactionExpirySpec.tests
         SchedulerTests.BlockEnergyLimitSpec.tests
         SchedulerTests.TransactionGroupingSpec2.tests
         SchedulerTests.SimpleTransferSpec.tests
         SchedulerTests.UpdateAccountKeys.tests
         SchedulerTests.UpdateCredentials.tests
         SchedulerTests.TransfersWithScheduleTest.tests
         SchedulerTests.EncryptedTransfersTest.tests
         SchedulerTests.MaxIncomingAmountsTest.tests
         SchedulerTests.StakedAmountLocked.tests
         SchedulerTests.RejectReasons.tests
         SchedulerTests.RejectReasonsRustContract.tests
