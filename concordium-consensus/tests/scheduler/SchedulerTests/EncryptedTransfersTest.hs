{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module SchedulerTests.EncryptedTransfersTest where

import qualified Test.HUnit as HUnit
import Test.Hspec

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.Runner as Runner

import Concordium.GlobalState.Basic.BlockState.Account
import Concordium.GlobalState.Basic.BlockState.Accounts as Acc
import Concordium.GlobalState.Basic.BlockState

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData
import Concordium.Crypto.EncryptedTransfers

import SchedulerTests.TestUtils

import Lens.Micro.Platform

import Concordium.Crypto.FFIDataTypes (ElgamalSecond, ElgamalSecondSecret)
import Data.Word (Word64)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Concordium.ID.DummyData (dummyEncryptionSecretKey)
import Data.Maybe (fromJust)
import Concordium.ID.Types (AccountEncryptionKey(..),CredentialRegistrationID(RegIdCred))
import Concordium.Scheduler.Types (unhashed)
import qualified Data.Sequence as Seq

-- This test will perform the following transactions and check that the resulting
-- blockstate is correct:
--
--- |----------------------------------+-----------------+-----------+---------------|
--- | After transaction                |                 |      Ales |        Thomas |
--- |----------------------------------+-----------------+-----------+---------------|
--- | A, PubToSec(1000)                | selfAmount      |      1000 |             0 |
--- |                                  | startIdx        |         0 |             0 |
--- |                                  | incomingAmounts |        [] |            [] |
--- |----------------------------------+-----------------+-----------+---------------|
--- | A->T, Send 100 from self         | selfAmount      |       900 |             0 |
--- |                                  | startIdx        |         0 |             0 |
--- |                                  | incomingAmounts |        [] |         [100] |
--- |----------------------------------+-----------------+-----------+---------------|
--- | A->T, Send 100 from self         | selfAmount      |       800 |             0 |
--- |                                  | startIdx        |         0 |             0 |
--- |                                  | incomingAmounts |        [] |     [100,100] |
--- |----------------------------------+-----------------+-----------+---------------|
--- | A->T, Send 100 from self         | selfAmount      |       700 |             0 |
--- |                                  | startIdx        |         0 |             0 |
--- |                                  | incomingAmounts |        [] | [100,100,100] |
--- |----------------------------------+-----------------+-----------+---------------|
--- | T->A, Send 150 combining up to 2 | selfAmount      |       700 |            50 |
--- |                                  | startIdx        |         0 |             2 |
--- |                                  | incomingAmounts |     [150] |         [100] |
--- |----------------------------------+-----------------+-----------+---------------|
--- | T->A, Send 150 combining up to 3 | selfAmount      |       700 |             0 |
--- |                                  | startIdx        |         0 |             3 |
--- |                                  | incomingAmounts | [150,150] |            [] |
--- |----------------------------------+-----------------+-----------+---------------|
--- | A, SecToPub(650)                 | selfAmount      |        50 |             0 |
--- |                                  | startIdx        |         0 |             3 |
--- |                                  | incomingAmounts | [150,150] |            [] |
--- |----------------------------------+-----------------+-----------+---------------|
--- | A, SecToPub(150)                 | selfAmount      |        50 |             0 |
--- |                                  | startIdx        |         1 |             3 |
--- |                                  | incomingAmounts |     [150] |            [] |
--- |----------------------------------+-----------------+-----------+---------------|
--- | A, SecToPub(200)                 | selfAmount      |         0 |             0 |
--- |                                  | startIdx        |         2 |             3 |
--- |                                  | incomingAmounts |        [] |            [] |
--- |----------------------------------+-----------------+-----------+---------------|


initialBlockState :: BlockState
initialBlockState = blockStateWithAlesAccount
    10000000000
    (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 10000000000) Acc.emptyAccounts)

alesEncryptionSecretKey :: ElgamalSecondSecret
alesEncryptionSecretKey = dummyEncryptionSecretKey alesAccount
alesEncryptionPublicKeyUnwrapped :: ElgamalSecond
alesEncryptionPublicKeyUnwrapped = let AccountEncryptionKey (RegIdCred s) = alesEncryptionPublicKey in s
alesEncryptionPublicKey :: AccountEncryptionKey
alesEncryptionPublicKey = (fromJust $ Acc.getAccount alesAccount (initialBlockState ^. blockAccounts)) ^. accountPersisting . accountEncryptionKey

thomasEncryptionSecretKey :: ElgamalSecondSecret
thomasEncryptionSecretKey = dummyEncryptionSecretKey thomasAccount
thomasEncryptionPublicKeyUnwrapped :: ElgamalSecond
thomasEncryptionPublicKeyUnwrapped = let AccountEncryptionKey (RegIdCred s) = thomasEncryptionPublicKey in s
thomasEncryptionPublicKey :: AccountEncryptionKey
thomasEncryptionPublicKey = (fromJust $ Acc.getAccount thomasAccount (initialBlockState ^. blockAccounts)) ^. accountPersisting . accountEncryptionKey

createEncryptedTransferData :: ElgamalSecond -> ElgamalSecondSecret -> AggregatedDecryptedAmount -> Word64 -> Maybe EncryptedAmountTransferData
createEncryptedTransferData second secret aggDecAmount amount = unsafeDupablePerformIO $ makeEncryptedAmountTransferData (initialBlockState ^. blockCryptographicParameters . unhashed) second secret aggDecAmount amount

createSecToPubTransferData :: ElgamalSecondSecret -> AggregatedDecryptedAmount -> Word64 -> Maybe SecToPubAmountTransferData
createSecToPubTransferData secret aggAmount amount = unsafeDupablePerformIO $ makeSecToPubAmountTransferData (initialBlockState ^. blockCryptographicParameters . unhashed) secret aggAmount amount

-- Transaction 1. Pub to sec (1000)
encryptedAmount1000 :: EncryptedAmount
encryptedAmount1000 = encryptAmountZeroRandomness (initialBlockState ^. blockCryptographicParameters . unhashed) 1000



-- Transaction 2. EncTransfer (A->T, 100) with previous amounts
aggregatedDecryptedAmount1 :: AggregatedDecryptedAmount
aggregatedDecryptedAmount1 = makeAggregatedDecryptedAmount encryptedAmount1000 1000 0
encryptedTransferData1 :: EncryptedAmountTransferData
encryptedTransferData1 = fromJust $ createEncryptedTransferData thomasEncryptionPublicKeyUnwrapped alesEncryptionSecretKey aggregatedDecryptedAmount1 100
incomingAmounts1 :: Seq.Seq EncryptedAmount
incomingAmounts1 = Seq.singleton $ eatdTransferAmount encryptedTransferData1

-- Transaction 3. EncTransfer (A->T, 100) with previous amounts
aggregatedDecryptedAmount2 :: AggregatedDecryptedAmount
aggregatedDecryptedAmount2 = makeAggregatedDecryptedAmount (eatdRemainingAmount encryptedTransferData1) 900 0
encryptedTransferData2 :: EncryptedAmountTransferData
encryptedTransferData2 = fromJust $ createEncryptedTransferData thomasEncryptionPublicKeyUnwrapped alesEncryptionSecretKey aggregatedDecryptedAmount2 100
incomingAmounts2 :: Seq.Seq EncryptedAmount
incomingAmounts2 = incomingAmounts1 Seq.:|> eatdTransferAmount encryptedTransferData2

-- Transaction 4. EncTransfer (A->T, 100) with previous amounts
aggregatedDecryptedAmount3 :: AggregatedDecryptedAmount
aggregatedDecryptedAmount3 = makeAggregatedDecryptedAmount (eatdRemainingAmount encryptedTransferData2) 800 0
encryptedTransferData3 :: EncryptedAmountTransferData
encryptedTransferData3 = fromJust $ createEncryptedTransferData thomasEncryptionPublicKeyUnwrapped alesEncryptionSecretKey aggregatedDecryptedAmount3 100
incomingAmounts3 :: Seq.Seq EncryptedAmount
incomingAmounts3 = incomingAmounts2 Seq.:|> eatdTransferAmount encryptedTransferData3

-- Transaction 5. EncTransfer (T->A, 150) with previous amounts
aggregatedEncryptedAmount4 :: EncryptedAmount
aggregatedEncryptedAmount4 = aggregateAmounts mempty $ aggregateAmounts (eatdTransferAmount encryptedTransferData1) (eatdTransferAmount encryptedTransferData2)
aggregatedDecryptedAmount4 :: AggregatedDecryptedAmount
aggregatedDecryptedAmount4 = makeAggregatedDecryptedAmount aggregatedEncryptedAmount4 200 2
encryptedTransferData4 :: EncryptedAmountTransferData
encryptedTransferData4 = fromJust $ createEncryptedTransferData alesEncryptionPublicKeyUnwrapped thomasEncryptionSecretKey aggregatedDecryptedAmount4 150
incomingAmounts4A, incomingAmounts4T :: Seq.Seq EncryptedAmount
incomingAmounts4A = Seq.singleton $ eatdTransferAmount encryptedTransferData4
incomingAmounts4T = Seq.singleton $ eatdTransferAmount encryptedTransferData3

-- Transaction 6. EncTransfer (T->A, 150) with previous amounts
aggregatedEncryptedAmount5 :: EncryptedAmount
aggregatedEncryptedAmount5 = aggregateAmounts (eatdRemainingAmount encryptedTransferData4) (eatdTransferAmount encryptedTransferData3)
aggregatedDecryptedAmount5 :: AggregatedDecryptedAmount
aggregatedDecryptedAmount5 = makeAggregatedDecryptedAmount aggregatedEncryptedAmount5 150 3
encryptedTransferData5 :: EncryptedAmountTransferData
encryptedTransferData5 = fromJust $ createEncryptedTransferData alesEncryptionPublicKeyUnwrapped thomasEncryptionSecretKey aggregatedDecryptedAmount5 150
incomingAmounts5A, incomingAmounts5T :: Seq.Seq EncryptedAmount
incomingAmounts5A = incomingAmounts4A Seq.:|> eatdTransferAmount encryptedTransferData5
incomingAmounts5T = Seq.empty

-- Transaction 7. Sec to Pub 650 (to not consume fully the selfAmount which is 700 rn)
aggregatedDecryptedAmount6 :: AggregatedDecryptedAmount
aggregatedDecryptedAmount6 = makeAggregatedDecryptedAmount (eatdRemainingAmount encryptedTransferData3) 700 0
secToPubTransferData1 :: SecToPubAmountTransferData
secToPubTransferData1 = fromJust $ createSecToPubTransferData alesEncryptionSecretKey aggregatedDecryptedAmount6 650
valid1 :: Bool
valid1 = verifySecretToPublicTransferProof (initialBlockState ^. blockCryptographicParameters . unhashed) alesEncryptionPublicKey (eatdRemainingAmount encryptedTransferData3) secToPubTransferData1

-- Transaction 8. Sec to Pub 150
aggregatedEncryptedAmount7 :: EncryptedAmount
aggregatedEncryptedAmount7 = aggregateAmounts (stpatdRemainingAmount secToPubTransferData1) (eatdTransferAmount encryptedTransferData4)
aggregatedDecryptedAmount7 :: AggregatedDecryptedAmount
aggregatedDecryptedAmount7 = makeAggregatedDecryptedAmount aggregatedEncryptedAmount7 200 1
secToPubTransferData2 :: SecToPubAmountTransferData
secToPubTransferData2 = fromJust $ createSecToPubTransferData alesEncryptionSecretKey aggregatedDecryptedAmount7 150
incomingAmounts7A :: Seq.Seq EncryptedAmount
incomingAmounts7A = Seq.singleton $ eatdTransferAmount encryptedTransferData5

-- Transaction 8. Sec to Pub 200
aggregatedEncryptedAmount8 :: EncryptedAmount
aggregatedEncryptedAmount8 = aggregateAmounts (stpatdRemainingAmount secToPubTransferData2) (eatdTransferAmount encryptedTransferData5)
aggregatedDecryptedAmount8 :: AggregatedDecryptedAmount
aggregatedDecryptedAmount8 = makeAggregatedDecryptedAmount aggregatedEncryptedAmount8 200 2
secToPubTransferData3 :: SecToPubAmountTransferData
secToPubTransferData3 = fromJust $ createSecToPubTransferData alesEncryptionSecretKey aggregatedDecryptedAmount8 200
incomingAmounts8A :: Seq.Seq EncryptedAmount
incomingAmounts8A = Seq.empty

testCases :: [TestCase]
testCases =
  [ TestCase
    { tcName = "Makes an encrypted transfer"
    , tcParameters = defaultParams { tpInitialBlockState = initialBlockState }
    , tcTransactions =
      [ ( Runner.TJSON { payload = Runner.TransferToEncrypted 1000
                         , metadata = makeDummyHeader alesAccount 1 100000
                         , keys = [(0, alesKP)]
                         }
          , (SuccessE [Types.EncryptedSelfAmountAdded {
                          eaaAccount = alesAccount,
                          eaaNewAmount = encryptedAmount1000,
                          eaaAmount = 1000
                          }
                      ], checkEncryptedBalance initialAccountEncryptedAmount{_selfAmount = encryptedAmount1000} alesAccount
            )
          ),
        let EncryptedAmountTransferData{..} = encryptedTransferData1
        in
         ( Runner.TJSON { payload = Runner.EncryptedAmountTransfer thomasAccount encryptedTransferData1
                       , metadata = makeDummyHeader alesAccount 2 100000
                       , keys = [(0, alesKP)]
                       }
        , (SuccessE [Types.EncryptedAmountsRemoved {
                        earAccount = alesAccount,
                        earUpToIndex = 0,
                        earNewAmount = eatdRemainingAmount
                        },
                      Types.NewEncryptedAmount {
                        neaAccount = thomasAccount,
                        neaNewIndex = 0,
                        neaEncryptedAmount = eatdTransferAmount
                        }
                    ], \bs -> do
                         checkEncryptedBalance initialAccountEncryptedAmount{_selfAmount = eatdRemainingAmount} alesAccount bs
                         checkEncryptedBalance initialAccountEncryptedAmount{_startIndex = 0,
                                                                             _incomingEncryptedAmounts = incomingAmounts1} thomasAccount bs
          )
        ),
        let EncryptedAmountTransferData{..} = encryptedTransferData2
        in
         ( Runner.TJSON { payload = Runner.EncryptedAmountTransfer thomasAccount encryptedTransferData2
                       , metadata = makeDummyHeader alesAccount 3 100000
                       , keys = [(0, alesKP)]
                       }
        , (SuccessE [Types.EncryptedAmountsRemoved {
                        earAccount = alesAccount,
                        earUpToIndex = 0,
                        earNewAmount = eatdRemainingAmount
                        },
                      Types.NewEncryptedAmount {
                        neaAccount = thomasAccount,
                        neaNewIndex = 1,
                        neaEncryptedAmount = eatdTransferAmount
                        }
                    ], \bs -> do
                         checkEncryptedBalance initialAccountEncryptedAmount{_selfAmount = eatdRemainingAmount} alesAccount bs
                         checkEncryptedBalance initialAccountEncryptedAmount{_startIndex = 0,
                                                                             _incomingEncryptedAmounts = incomingAmounts2 } thomasAccount bs
          )
        ),
        let EncryptedAmountTransferData{..} = encryptedTransferData3
        in
         ( Runner.TJSON { payload = Runner.EncryptedAmountTransfer thomasAccount encryptedTransferData3
                       , metadata = makeDummyHeader alesAccount 4 100000
                       , keys = [(0, alesKP)]
                       }
        , (SuccessE [Types.EncryptedAmountsRemoved {
                        earAccount = alesAccount,
                        earUpToIndex = 0,
                        earNewAmount = eatdRemainingAmount
                        },
                      Types.NewEncryptedAmount {
                        neaAccount = thomasAccount,
                        neaNewIndex = 2,
                        neaEncryptedAmount = eatdTransferAmount
                        }
                    ], \bs -> do
                         checkEncryptedBalance initialAccountEncryptedAmount{_selfAmount = eatdRemainingAmount} alesAccount bs
                         checkEncryptedBalance initialAccountEncryptedAmount{_startIndex = 0,
                                                                             _incomingEncryptedAmounts = incomingAmounts3 } thomasAccount bs
          )
        ),
        ( Runner.TJSON { payload = Runner.EncryptedAmountTransfer alesAccount encryptedTransferData4
                       , metadata = makeDummyHeader thomasAccount 1 100000
                       , keys = [(0, thomasKP)]
                       }
        , (SuccessE [Types.EncryptedAmountsRemoved {
                        earAccount = thomasAccount,
                        earUpToIndex = 2,
                        earNewAmount = eatdRemainingAmount encryptedTransferData4
                        },
                      Types.NewEncryptedAmount {
                        neaAccount = alesAccount,
                        neaNewIndex = 0,
                        neaEncryptedAmount = eatdTransferAmount encryptedTransferData4
                        }
                    ], \bs -> do
                         checkEncryptedBalance initialAccountEncryptedAmount{_selfAmount = eatdRemainingAmount encryptedTransferData3,
                                                                             _startIndex = 0,
                                                                             _incomingEncryptedAmounts = incomingAmounts4A} alesAccount bs
                         checkEncryptedBalance initialAccountEncryptedAmount{_selfAmount = eatdRemainingAmount encryptedTransferData4,
                                                                             _startIndex = 2,
                                                                             _incomingEncryptedAmounts = incomingAmounts4T } thomasAccount bs
          )
        ),
        ( Runner.TJSON { payload = Runner.EncryptedAmountTransfer alesAccount encryptedTransferData5
                       , metadata = makeDummyHeader thomasAccount 2 100000
                       , keys = [(0, thomasKP)]
                       }
        , (SuccessE [Types.EncryptedAmountsRemoved {
                        earAccount = thomasAccount,
                        earUpToIndex = 3,
                        earNewAmount = eatdRemainingAmount encryptedTransferData5
                        },
                      Types.NewEncryptedAmount {
                        neaAccount = alesAccount,
                        neaNewIndex = 1,
                        neaEncryptedAmount = eatdTransferAmount encryptedTransferData5
                        }
                    ], \bs -> do
                         checkEncryptedBalance initialAccountEncryptedAmount{_selfAmount = eatdRemainingAmount encryptedTransferData3,
                                                                             _startIndex = 0,
                                                                             _incomingEncryptedAmounts = incomingAmounts5A} alesAccount bs
                         checkEncryptedBalance initialAccountEncryptedAmount{_selfAmount = eatdRemainingAmount encryptedTransferData5,
                                                                             _startIndex = 3,
                                                                             _incomingEncryptedAmounts = incomingAmounts5T } thomasAccount bs
          )
        ),
        ( Runner.TJSON { payload = Runner.TransferToPublic secToPubTransferData1
                       , metadata = makeDummyHeader alesAccount 5 100000
                       , keys = [(0, alesKP)]
                       }
        , (SuccessE [Types.EncryptedAmountsRemoved {
                        earAccount = alesAccount,
                        earUpToIndex = 0,
                        earNewAmount = stpatdRemainingAmount secToPubTransferData1
                        },
                      Types.AmountAddedByDecryption {
                        aabdAccount = alesAccount,
                        aabdAmount = 650
                        }
                    ], checkEncryptedBalance initialAccountEncryptedAmount{_selfAmount = stpatdRemainingAmount secToPubTransferData1,
                                                                             _startIndex = 0,
                                                                             _incomingEncryptedAmounts = incomingAmounts5A} alesAccount
          )
        ),
        ( Runner.TJSON { payload = Runner.TransferToPublic secToPubTransferData2
                       , metadata = makeDummyHeader alesAccount 6 100000
                       , keys = [(0, alesKP)]
                       }
        , (SuccessE [Types.EncryptedAmountsRemoved {
                        earAccount = alesAccount,
                        earUpToIndex = 1,
                        earNewAmount = stpatdRemainingAmount secToPubTransferData2
                        },
                      Types.AmountAddedByDecryption {
                        aabdAccount = alesAccount,
                        aabdAmount = 150
                        }
                    ], checkEncryptedBalance initialAccountEncryptedAmount{_selfAmount = stpatdRemainingAmount secToPubTransferData2,
                                                                             _startIndex = 1,
                                                                             _incomingEncryptedAmounts = incomingAmounts7A} alesAccount
          )
        ),
        ( Runner.TJSON { payload = Runner.TransferToPublic secToPubTransferData3
                       , metadata = makeDummyHeader alesAccount 7 100000
                       , keys = [(0, alesKP)]
                       }
        , (SuccessE [Types.EncryptedAmountsRemoved {
                        earAccount = alesAccount,
                        earUpToIndex = 2,
                        earNewAmount = stpatdRemainingAmount secToPubTransferData3
                        },
                      Types.AmountAddedByDecryption {
                        aabdAccount = alesAccount,
                        aabdAmount = 200
                        }
                    ], checkEncryptedBalance initialAccountEncryptedAmount{_selfAmount = stpatdRemainingAmount secToPubTransferData3,
                                                                             _startIndex = 2,
                                                                             _incomingEncryptedAmounts = incomingAmounts8A} alesAccount
          )
        )
      ]
     }
  ]
  where checkEncryptedBalance accEncAmount acc = (\bs -> specify ("Correct final balance on " ++ show acc) $
           case Acc.getAccount acc (bs ^. blockAccounts) of
             Nothing -> HUnit.assertFailure $ "Account with id '" ++ show acc ++ "' not found"
             Just account -> HUnit.assertEqual "Expected encrypted amount matches"  accEncAmount (account ^. accountEncryptedAmount))

tests :: Spec
tests = describe "TransferToEncrypted." $ mkSpecs testCases
