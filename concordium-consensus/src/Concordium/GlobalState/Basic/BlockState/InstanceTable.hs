{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Concordium.GlobalState.Basic.BlockState.InstanceTable where

import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Instance

import Data.Word
import Lens.Micro.Platform
import Lens.Micro.Internal (Ixed,Index,IxValue)
import Data.Serialize

data InstanceTable
    -- |The empty instance table
    = Empty
    -- |A non-empty instance table (recording the number of instances present)
    | Tree !Word64 !IT
    deriving (Show)

computeBranchHash :: IT -> IT -> H.Hash
computeBranchHash t1 t2 = H.hashShort $! (H.hashToShortByteString (getHash t1 :: H.Hash) <> H.hashToShortByteString (getHash t2 :: H.Hash))

-- |Internal tree nodes of an 'InstanceTable'.
-- Branches satisfy the following invariant properties:
-- * The left branch is always a full sub-tree with height 1 less than the parent (or a leaf if the parent height is 0)
-- * The right branch has height less than the parent
-- * The hash is @computeBranchHash l r@ where @l@ and @r@ are the left and right subtrees
-- * The first @Bool@ is @True@ if the tree is full, i.e. the right sub-tree is full with height 1 less than the parent
-- * The second @Bool@ is @True@ if the tree has vacant leaves: either @hasVacancies l@ or @hasVacancies r@ is @True@
data IT
    -- |A branch has the following fields:
    -- * the height of the branch (0 if all children are leaves)
    -- * whether the branch is a full binary tree
    -- * whether the tree has vacant leaves
    -- * the Merkle hash (lazy)
    -- * the left and right subtrees
    = Branch !Word8 !Bool !Bool H.Hash IT IT
    -- |A leaf holds a contract instance
    | Leaf !Instance
    -- |A vacant leaf records the 'ContractSubindex' of the last instance
    -- with this 'ContractIndex'.
    | VacantLeaf !ContractSubindex
    deriving (Show)

instance HashableTo H.Hash IT where
    getHash (Branch _ _ _ h _ _) = h
    getHash (Leaf i) = getHash i
    getHash (VacantLeaf si) = H.hash $ runPut $ put si

instance HashableTo H.Hash InstanceTable where
    -- The hash of the empty tree is defined arbitrarily to be the hash of the string "EmptyInstances"
    getHash Empty = H.hash "EmptyInstances"
    getHash (Tree _ t) = getHash t

-- |A fold over the leaves of an 'IT'
foldIT :: SimpleFold IT (Either ContractSubindex Instance)
foldIT up (Branch _ _ _ _ l r) = foldIT up l <> foldIT up r
foldIT up t@(Leaf i) = t <$ up (Right i)
foldIT up t@(VacantLeaf si) = t <$ up (Left si)

type instance Index IT = ContractIndex
type instance IxValue IT = Instance

instance Ixed IT where
    ix i upd br@(Branch b f v _ t1 t2)
        | i < 2^b = mkBranch <$> ix i upd t1 <*> pure t2
        | i < 2^(b+1) = mkBranch t1 <$> (ix (i - 2^b) upd t2)
        | otherwise = pure br
        where
            mkBranch t1' t2' = Branch b f v (computeBranchHash t1' t2') t1' t2'
    ix i upd l@(Leaf inst)
        | i == 0    = Leaf <$> upd inst
        | otherwise = pure l
    ix _ _ v@(VacantLeaf _) = pure v

type instance Index InstanceTable = ContractAddress
type instance IxValue InstanceTable = Instance

instance Ixed InstanceTable where
    ix _ _ t@Empty = pure t
    ix i upd (Tree s t) = Tree s <$> (ix (contractIndex i) . filtered ((== i) . instanceAddress . instanceParameters)) upd t

-- |Determine if an 'IT' is a full binary tree.
isFull :: IT -> Bool
isFull (Branch _ f _ _ _ _) = f
isFull _ = True

-- |The height for the level above.
nextHeight :: IT -> Word8
nextHeight (Branch h _ _ _ _ _) = h + 1
nextHeight _ = 0

hasVacancies :: IT -> Bool
hasVacancies (Branch _ _ v _ _ _) = v
hasVacancies (Leaf _) = False
hasVacancies (VacantLeaf _) = True

newContractInstance :: Lens InstanceTable InstanceTable ContractAddress Instance
newContractInstance mk Empty = Tree 1 . Leaf <$> mk (ContractAddress 0 0)
newContractInstance mk (Tree s0 t0) = Tree (s0 + 1) <$> nci 0 t0
    where
        -- Insert into a tree with vacancies: insert in left if it has vacancies, otherwise right
        nci offset (Branch h f True _ l r)
            | hasVacancies l = let newBranch l' = mkBranch h f (hasVacancies l' || hasVacancies r) l' r in newBranch <$> nci offset l
            | hasVacancies r = let newBranch r' = mkBranch h f (hasVacancies r') l r' in newBranch <$> nci (offset + 2^h) r
            | otherwise = error "newContractInstance: branch has vacancies, but children do not"
        -- Insert into full tree with no vacancies: create new branch at top level
        nci offset b@(Branch h True False _ _ _) = mkBranch (h+1) False False b <$> (leaf (offset + 2^(h+1)) 0)
        -- Insert into non-full tree with no vacancies: insert on right subtree (invariant implies left is full, but can add to right)
        nci offset (Branch h False False _ l r) = newBranch <$> nci (offset + 2^h) r
            where
                newBranch r' = mkBranch h (isFull r' && nextHeight r' == h) False l r'
        -- Insert at leaf: create a new branch
        nci offset b@(Leaf _) = mkBranch 0 True False b <$> (leaf (offset + 1) 0)
        -- Insert at vacant leaf: create leaf with next subindex
        nci offset (VacantLeaf si) = leaf offset (succ si)
        mkBranch h f v t1' t2' = Branch h f v (computeBranchHash t1' t2') t1' t2'
        leaf ind subind = Leaf <$> mk (ContractAddress ind subind)

-- |Delete the contract instance with the given 'ContractIndex'.
deleteContractInstance :: ContractIndex -> InstanceTable -> InstanceTable
deleteContractInstance _ Empty = Empty
deleteContractInstance i0 (Tree s0 t0) = uncurry Tree $ dci i0 t0
    where
        dci i l@(Leaf inst)
            | i == 0 = (s0 - 1, VacantLeaf $ contractSubindex $ instanceAddress $ instanceParameters inst)
            | otherwise = (s0, l)
        dci _ vl@(VacantLeaf _) = (s0, vl)
        dci i b@(Branch h f _ _ l r)
            | i < 2^h = let (s', l') = dci i l in (s', mkBranch l' r)
            | i < 2^(h+1) = let (s', r') = dci (i - 2^h) r in (s', mkBranch l r')
            | otherwise = (s0, b)
            where
                mkBranch t1' t2' = Branch h f (hasVacancies t1' || hasVacancies t2') (computeBranchHash t1' t2') t1' t2'

-- |Delete the contract instance at the given 'ContractAddress'.
deleteContractInstanceExact :: ContractAddress -> InstanceTable -> InstanceTable
deleteContractInstanceExact _ Empty = Empty
deleteContractInstanceExact addr (Tree s0 t0) = uncurry Tree $ dci (contractIndex addr) t0
    where
        dci i l@(Leaf inst)
            | i == 0 && addr == instanceAddress (instanceParameters inst)
                        = (s0 - 1, VacantLeaf $ contractSubindex $ instanceAddress $ instanceParameters inst)
            | otherwise = (s0, l)
        dci _ vl@(VacantLeaf _) = (s0, vl)
        dci i b@(Branch h f _ _ l r)
            | i < 2^h = let (s', l') = dci i l in (s', mkBranch l' r)
            | i < 2^(h+1) = let (s', r') = dci (i - 2^h) r in (s', mkBranch l r')
            | otherwise = (s0, b)
            where
                mkBranch t1' t2' = Branch h f (hasVacancies t1' || hasVacancies t2') (computeBranchHash t1' t2') t1' t2'

-- |Construct an 'InstanceTable' given a monadic function that
-- will be invoked for each 'ContractIndex' in sequence to give
-- the 'ContractSubindex' (for a vacancy) or 'Instance', until
-- the function returns 'Nothing', indicating there are no more
-- instances in the constructed table.
constructM :: (Monad m)
    => (ContractIndex -> m (Maybe (Either ContractSubindex Instance)))
    -> m InstanceTable
constructM build = c 0 0 []
    where
        -- The list argument is a stack of @Maybe IT@ such that, for each @Just t@ in the list:
        -- * @t@ is full (satisfying the invariant properties of 'IT' instances);
        -- * The height of @t@ is one less than its index in the list.
        c !idx !count l = build idx >>= \case
            Nothing -> return $! collapse0 count l
            Just (Left si) -> c (idx + 1) count $! bubble (VacantLeaf si) l
            Just (Right inst) -> c (idx + 1) (count + 1) $! bubble (Leaf inst) l
        -- Add a new entry to the stack. @t@ is always a full 'IT' at level one
        -- less than that required for the next index of the stack.
        bubble t [] = [Just t]
        bubble t (Nothing : l) = Just t : l
        bubble t (Just t' : l) = Nothing : (bubble $! mkBranch True t' t) l
        -- Collapse a stack with the above invariant properties into an 'InstanceTable'.
        collapse0 _ [] = Empty
        collapse0 count (Nothing:l) = collapse0 count l
        collapse0 count (Just t:l) = collapse1 count l t
        -- Here @t@ is either a non-full tree at the appropriate level of the stack,
        -- or is a full tree at a lower level.
        collapse1 count [] t = Tree count t
        collapse1 count (Nothing:l) t = collapse1 count l t
        collapse1 count (Just t':l) t = collapse1 count l (mkBranch False t' t)
        mkBranch f t1' t2' = Branch (nextHeight t1') f (hasVacancies t1' || hasVacancies t2') (computeBranchHash t1' t2') t1' t2'

-- |A collection of smart contract instances.
newtype Instances = Instances {
  _instances :: InstanceTable
  }
makeLenses ''Instances

instance HashableTo H.Hash Instances where
    getHash = getHash . _instances

type instance Index Instances = ContractAddress
type instance IxValue Instances = Instance

instance Ixed Instances where
    ix z = instances . ix z

instance Show Instances where
    show (Instances Empty) = "Instances {}"
    show (Instances (Tree _ t)) = "Instances {\n" ++ (concatMap f $ t ^.. foldIT) ++ "}"
        where
            f (Left _) = ""
            f (Right inst) = show inst <> "\n"
