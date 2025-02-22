module Concordium.GlobalState.Basic.BlockState.Instances(
    InstanceParameters(..),
    Instance(..),
    makeInstance,
    iaddress,
    Instances,
    emptyInstances,
    getInstance,
    updateInstance,
    updateInstanceAt,
    updateInstanceAt',
    createInstance,
    deleteInstance,
    foldInstances,
    instanceCount,
    -- * Serialization
    putInstancesV0,
    getInstancesV0
) where

import Concordium.Types
import qualified Concordium.Wasm as Wasm
import Concordium.Types.Instance
import Concordium.GlobalState.Basic.BlockState.InstanceTable

import Data.Serialize
import qualified Data.Set as Set
import Data.Word
import Lens.Micro.Platform

-- |The empty set of smart contract instances.
emptyInstances :: Instances
emptyInstances = Instances Empty

-- |Get the smart contract instance at the given address, if it exists.
getInstance :: ContractAddress -> Instances -> Maybe Instance
getInstance addr (Instances iss) = iss ^? ix addr

-- |Update the instance at the specified address with an amount delta and value.
-- If there is no instance with the given address, this does nothing.
updateInstanceAt :: ContractAddress -> AmountDelta -> Wasm.ContractState -> Instances -> Instances
updateInstanceAt ca amt val (Instances iss) = Instances (iss & ix ca %~ updateInstance amt val)

-- |Update the instance at the specified address with a __new amount__ and value.
-- If there is no instance with the given address, this does nothing.
updateInstanceAt' :: ContractAddress -> Amount -> Wasm.ContractState -> Instances -> Instances
updateInstanceAt' ca amt val (Instances iss) = Instances (iss & ix ca %~ updateInstance' amt val)

-- |Create a new smart contract instance.
createInstance :: (ContractAddress -> Instance) -> Instances -> (Instance, Instances)
createInstance mkInst (Instances iss) = Instances <$> (iss & newContractInstance <%~ mkInst)

-- |Delete the instance with the given address.  Does nothing
-- if there is no such instance.
deleteInstance :: ContractAddress -> Instances -> Instances
deleteInstance ca (Instances i) = Instances (deleteContractInstanceExact ca i)

-- |A fold over smart contract instances.
foldInstances :: SimpleFold Instances Instance
foldInstances _ is@(Instances Empty) = is <$ mempty
foldInstances f is@(Instances (Tree _ t)) = is <$ (foldIT . _Right) f t

instanceCount :: Instances -> Word64
instanceCount (Instances Empty) = 0
instanceCount (Instances (Tree c _)) = c

-- |Serialize 'Instances' in V0 format.
putInstancesV0 :: Putter Instances
putInstancesV0 (Instances Empty) = putWord8 0
putInstancesV0 (Instances (Tree _ t)) = do
        mapM_ putOptInstance (t ^.. foldIT)
        putWord8 0
    where
        putOptInstance (Left si) = do
            putWord8 1
            put si
        putOptInstance (Right inst) = do
            putWord8 2
            putInstanceV0 inst

-- |Deserialize 'Instances' in V0 format.
getInstancesV0
    :: (ModuleRef -> Wasm.InitName -> Maybe (Set.Set Wasm.ReceiveName, Wasm.ModuleInterface))
    -> Get Instances
getInstancesV0 resolve = Instances <$> constructM buildInstance
    where
        buildInstance idx = getWord8 >>= \case
            0 -> return Nothing
            1 -> Just . Left <$> get
            2 -> Just . Right <$> getInstanceV0 resolve idx
            _ -> fail "Bad instance list"
