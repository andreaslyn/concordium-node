{-# LANGUAGE RecordWildCards, DeriveGeneric, GeneralizedNewtypeDeriving #-}
module Concordium.Types where

import GHC.Generics
import qualified Data.Map as Map
import Data.Word
import Data.ByteString
import Data.Serialize.Put
import Data.Serialize
import Data.Hashable

import qualified Concordium.Crypto.DummySignature as Sig
import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Crypto.DummyVRF as VRF

import Concordium.Payload.Transaction(GlobalState, initState)

newtype Slot = Slot Word64 deriving (Eq, Ord, Num, Real, Enum, Integral, Show, Serialize)
type BlockHash = Hash.Hash
type BakerId = Word64
type BlockProof = VRF.Proof
type BlockSignature = Sig.Signature
type BlockNonce = (VRF.Hash, VRF.Proof)
type BlockData = ByteString
newtype BlockHeight = BlockHeight Word64 deriving (Eq, Ord, Num, Real, Enum, Integral, Show, Serialize)

type LeadershipElectionNonce = ByteString
type BakerSignVerifyKey = Sig.VerifyKey
type BakerSignPrivateKey = Sig.SignKey
type BakerElectionVerifyKey = VRF.PublicKey
type BakerElectionPrivateKey = VRF.PrivateKey
type LotteryPower = Double
type ElectionDifficulty = Double

type VoterId = Word64
type VoterVerificationKey = Sig.VerifyKey
type VoterVRFPublicKey = VRF.PublicKey
type VoterSignKey = Sig.SignKey
-- Using a floating point number for voter power may be a bad idea.
type VoterPower = Int

newtype FinalizationIndex = FinalizationIndex Word64 deriving (Eq, Ord, Num, Real, Enum, Integral, Show, Serialize)

data Block = Block {
    blockSlot :: Slot,
    blockPointer :: BlockHash,
    blockBaker :: BakerId,
    blockProof :: BlockProof,
    blockNonce :: BlockNonce,
    blockLastFinalized :: BlockHash,
    blockData :: BlockData,
    blockSignature :: BlockSignature
} deriving (Show)

serializeBlockBody ::
    Slot
    -> BlockHash
    -> BakerId
    -> BlockProof
    -> BlockNonce
    -> BlockHash
    -> BlockData
    -> Put
serializeBlockBody sl bpt bid bpf bn lfpt bdata = do
    put sl
    put bpt
    put bid
    put bpf
    put bn
    put lfpt
    put bdata

serializeBlock :: Block -> Put
serializeBlock Block{..} = do
    serializeBlockBody blockSlot blockPointer blockBaker blockProof blockNonce blockLastFinalized blockData
    put blockSignature

deserializeBlock :: Get Block
deserializeBlock = do
    blockSlot <- get
    blockPointer <- get
    blockBaker <- get
    blockProof <- get
    blockNonce <- get
    blockLastFinalized <- get
    blockData <- get
    blockSignature <- get
    return (Block {..})

signBlock :: 
    Sig.SignKey
    -> Slot
    -> BlockHash
    -> BakerId
    -> BlockProof
    -> BlockNonce
    -> BlockHash
    -> BlockData
    -> Block
signBlock ks blockSlot blockPointer blockBaker blockProof blockNonce blockLastFinalized blockData = Block {..}
    where
        blockSignature = Sig.sign ks $ runPut $
            serializeBlockBody blockSlot blockPointer blockBaker blockProof blockNonce blockLastFinalized blockData

verifyBlockSignature :: Sig.VerifyKey -> Block -> Bool
verifyBlockSignature kv Block{..} = Sig.verify kv bs blockSignature
    where
        bs = runPut $ serializeBlockBody blockSlot blockPointer blockBaker blockProof blockNonce blockLastFinalized blockData

hashBlock :: Block -> BlockHash
hashBlock = Hash.hashLazy . runPutLazy . serializeBlock

data BlockPointer = BlockPointer {
    bpHash :: !BlockHash,
    bpBlock :: !Block,
    bpParent :: BlockPointer,
    bpLastFinalized :: BlockPointer,
    bpHeight :: !BlockHeight,
    bpState :: !GlobalState
}

instance Eq BlockPointer where
    bp1 == bp2 = bpHash bp1 == bpHash bp2

instance Ord BlockPointer where
    compare bp1 bp2 = compare (bpHash bp1) (bpHash bp2)

instance Hashable BlockPointer where
    hashWithSalt s = hashWithSalt s . bpHash
    hash = hash . bpHash

data FinalizationProof = FinalizationProof [(Word32, Sig.Signature)]
    deriving (Eq)

emptyFinalizationProof :: FinalizationProof
emptyFinalizationProof = FinalizationProof []


data FinalizationRecord = FinalizationRecord {
    finalizationIndex :: FinalizationIndex,
    finalizationBlockPointer :: BlockHash,
    finalizationProof :: FinalizationProof,
    finalizationDelay :: BlockHeight
} deriving (Eq)
instance Serialize FinalizationRecord where
    put FinalizationRecord{..} = do
        put finalizationIndex
        put finalizationBlockPointer
        let FinalizationProof sigs = finalizationProof
        put sigs
        put finalizationDelay
    get = do
        finalizationIndex <- get
        finalizationBlockPointer <- get
        sigs <- get
        let finalizationProof = FinalizationProof sigs
        finalizationDelay <- get
        return $ FinalizationRecord{..}

data BakerInfo = BakerInfo {
    bakerElectionVerifyKey :: BakerElectionVerifyKey,
    bakerSignatureVerifyKey :: BakerSignVerifyKey,
    bakerLotteryPower :: LotteryPower
} deriving (Eq, Generic)
instance Serialize BakerInfo where

data BirkParameters = BirkParameters {
    birkLeadershipElectionNonce :: LeadershipElectionNonce,
    birkElectionDifficulty :: ElectionDifficulty,
    birkBakers :: Map.Map BakerId BakerInfo
} deriving (Eq, Generic)
instance Serialize BirkParameters where

birkBaker :: BakerId -> BirkParameters -> Maybe BakerInfo
birkBaker bid bps = Map.lookup bid (birkBakers bps)


data VoterInfo = VoterInfo {
    voterVerificationKey :: VoterVerificationKey,
    voterVRFKey :: VoterVRFPublicKey,
    voterPower :: VoterPower
} deriving (Eq, Generic)
instance Serialize VoterInfo where

data FinalizationParameters = FinalizationParameters [VoterInfo]
    deriving (Eq, Generic)
instance Serialize FinalizationParameters where

-- | Time in seconds since the epoch
type Timestamp = Word64
-- | Time duration in seconds
type Duration = Word64

data GenesisData = GenesisData {
    genesisTime :: Timestamp,
    genesisSlotDuration :: Duration,
    genesisBirkParameters :: BirkParameters,
    genesisFinalizationParameters :: FinalizationParameters
} deriving (Generic)

instance Serialize GenesisData where

makeGenesisBlock :: GenesisData -> Block
makeGenesisBlock genData = Block {
    blockSlot = 0,
    blockPointer = Hash.Hash empty,
    blockBaker = 0,
    blockProof = VRF.emptyProof,
    blockNonce = (VRF.emptyHash, VRF.emptyProof),
    blockLastFinalized = Hash.Hash empty,
    blockData = runPut $ put genData,
    blockSignature = Sig.emptySignature
}

makeGenesisBlockPointer :: GenesisData -> BlockPointer
makeGenesisBlockPointer genData = theBlockPointer
    where
        theBlockPointer = BlockPointer {..}
        bpBlock = makeGenesisBlock genData
        bpHash = hashBlock bpBlock
        bpParent = theBlockPointer
        bpLastFinalized = theBlockPointer
        bpHeight = 0
        bpState = initState 2
