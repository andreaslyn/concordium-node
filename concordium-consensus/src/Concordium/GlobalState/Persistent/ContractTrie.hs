
module Concordium.GlobalState.Persistent.ContractTrie where

import Data.Word
import Data.Maybe (isNothing)
import Data.Bifunctor (first)
import qualified Data.List as L

type Prefix = [Word8]
type Key = [Word8]
type Branch a = (Prefix, Trie a)

data Trie a = Trie { value :: Maybe a
                   , branches :: [Branch a] }
              deriving (Show)

empty :: Trie a
empty = Trie { value = Nothing, branches = [] }

isEmpty :: Trie a -> Bool
isEmpty trie = case trie of
  Trie { value = Nothing, branches = [] } -> True
  _ -> False

gotSingleBranch :: Trie a -> Bool
gotSingleBranch trie = case branches trie of
  [_] -> True
  _ -> False

valueNode :: a -> Trie a
valueNode value = Trie {value = Just value, branches = []}

branchNode :: [Branch a] -> Trie a
branchNode branches = Trie{value = Nothing, branches = branches}

singleton :: Prefix -> a -> Trie a
singleton prefix value =
  case prefix of
    [] -> valueNode value
    _ -> branchNode [ (prefix, valueNode value ) ]

tlookup :: Key -> Trie a -> Maybe a
tlookup key node =
    case key of
      [] -> value node
      (k : _) -> do (branchPrefix, branch) <- L.find ((== k) . head . fst) (branches node)
                    let (_, remainingKey, remainingBranchPrefix) = key `sharedPrefixOf` branchPrefix
                    if L.null remainingBranchPrefix then
                      tlookup remainingKey branch
                    else
                      Nothing

sharedPrefixOf :: Prefix -> Prefix -> (Prefix, Prefix, Prefix)
sharedPrefixOf keyL keyR =
    case (keyL, keyR) of
      (x:xs, y:ys) | x == y -> let (p, xs', ys') = sharedPrefixOf xs ys in (x:p, xs', ys')
      (xs, ys) -> ([], xs, ys)

-- | A generalized update function which can be used to insert, delete and update values of the map given some key.
-- The update function will receive the current value (Just a) or Nothing if the value is currently not in the map.
-- If the result of the function will replace the value, if the result is Nothing the key will be removed.
--
-- It will ensure to compress branches when inserting and removing keys.
--
-- Assumes each node have branches sorted by their prefix, likewise it ensures inserted branches follow this.
alter :: Key -> (Maybe a -> Maybe a) -> Trie a -> Trie a
alter key alterFn node =
    case key of
      -- No key left means we have found the current node for the key
      [] -> node { value = alterFn (value node) }
      -- Using the first part of the key we split the node branches
      (k : _) -> let (less, greaterOrEq) = L.span ((< k) . head . fst) (branches node)
              in case greaterOrEq of
                   -- No branch with prefix equal to this or higher
                   [] -> case alterFn Nothing of
                           Nothing -> node
                           Just value -> node {branches = less ++ [(key, valueNode value)] }
                   -- Some branches with prefix eq or higher
                   ((branchPrefix, branch):greater) ->
                       let (shared, remainingKey, remainingBranchPrefix) = key `sharedPrefixOf` branchPrefix in
                         if L.null shared then
                           -- If they share no prefix insert a new branch with this prefix.
                           case alterFn Nothing of
                             Nothing -> node
                             Just value ->
                               let newBranch = (remainingKey, valueNode value)
                               in node {branches = less ++ newBranch:greaterOrEq}
                         else if L.null remainingBranchPrefix then
                           -- If the key takes up all of the branch prefix we insert in the branch with the remaining key.
                           let updatedBranchNode = alter remainingKey alterFn branch
                           in if isEmpty updatedBranchNode then
                                -- If the updated branch is now empty we remove it.
                                node {branches = less ++ greater}
                              else if gotSingleBranch updatedBranchNode && isNothing (value updatedBranchNode) then
                                -- The updated branch have only a single branch and no value, so we compress.
                                let updatedSubBranch = head $ branches updatedBranchNode
                                    updatedBranch = first (branchPrefix ++) updatedSubBranch
                                in node {branches = less ++ updatedBranch:greater}
                              else
                                -- Otherwise we simply replace the branch with the updated one.
                                let updatedBranch = (branchPrefix, updatedBranchNode)
                                in node {branches = less ++ updatedBranch:greater}
                         else
                           case alterFn Nothing of
                             Nothing -> node
                             Just value ->
                               -- If the branch shared some prefix, but does not take up the branch prefix completely we insert new node with the shared part
                               let updatedBranch = (remainingBranchPrefix, branch)
                                   newBranch = (remainingKey, valueNode value)
                                   splitBranches = if head remainingKey < head remainingBranchPrefix then [newBranch, updatedBranch] else [updatedBranch, newBranch]
                                   splitBranch = (shared, branchNode splitBranches)
                               in node {branches = less ++ splitBranch:greater}

tinsert :: Key -> a -> Trie a -> Trie a
tinsert key value = alter key (const (Just value))

tdelete :: Key -> Trie a -> Trie a
tdelete key = alter key (const Nothing)

toList :: Trie a -> [(Key, a)]
toList trie = toList' [] trie
  where
    toList' prefix node =
      let rest = concatMap (\branch -> toList' (prefix ++ fst branch) (snd branch)) (branches node)
      in case value node of
        Just v -> (prefix, v) : rest
        Nothing -> rest


-- tinsert' :: Key -> a -> Trie a -> Trie a
-- tinsert' key value node =
--     case key of
--       [] -> node { value = Just value }
--       (k : _) -> let (less, greaterOrEq) = L.span ((< k) . head . fst) (branches node)
--               in case greaterOrEq of
--                    -- No branch with prefix equal to this or higher
--                    [] -> node {branches = less ++ [(key, valueNode value)] }
--                    -- Some branches with prefix eq or higher
--                    ((branchPrefix, branch):greater) ->
--                        let (shared, remainingKey, remainingBranchPrefix) = key `sharedPrefixOf` branchPrefix in
--                          if L.null shared then
--                            -- If they share no prefix insert a new branch with this prefix.
--                            let newBranch = (remainingKey, valueNode value)
--                            in node {branches = less ++ newBranch:greaterOrEq}
--                          else if L.null remainingBranchPrefix then
--                            -- If the key takes up all of the branch prefix we insert in the branch with the remaining key.
--                            let updatedBranchNode = tinsert remainingKey value branch
--                                updatedBranch = (remainingBranchPrefix, updatedBranchNode)
--                            in node {branches = less ++ updatedBranch:greater}
--                          else
--                            -- If the branch shared some prefix, but does not take up the branch prefix completely we insert new node with the shared part
--                            let updatedBranch = (remainingBranchPrefix, branch)
--                                newBranch = (remainingKey, valueNode value)
--                                splitBranches = if head remainingKey < head remainingBranchPrefix then [newBranch, updatedBranch] else [updatedBranch, newBranch]
--                                splitBranch = (shared, branchNode splitBranches)
--                            in node {branches = less ++ splitBranch:greater}

-- tdelete' :: Key -> Trie a -> Trie a
-- tdelete' key node =
--   case key of
--     [] -> node {value = Nothing}
--     (k : _) ->
--       let (less, greaterOrEq) = L.span ((< k) . head . fst) (branches node)
--       in case greaterOrEq of
--         -- No branch with prefix equal to this or higher
--         [] -> node
--         -- Some branches with prefix eq or higher
--         ((branchPrefix, branch):greater) ->
--           let (shared, remainingKey, remainingBranchPrefix) = key `sharedPrefixOf` branchPrefix in
--             if L.null shared then
--               -- If they share no prefix insert a new branch with this prefix.
--               node
--             else if L.null remainingBranchPrefix then
--               let updatedBranchNode = tdelete remainingKey branch
--               in if isEmpty updatedBranchNode then
--                    -- If the updated branch is now empty we remove it.
--                    node {branches = less ++ greater}
--                  else if gotSingleBranch updatedBranchNode && isNothing (value updatedBranchNode) then
--                    -- The updated branch have only a single branch and no value, so we compress.
--                    let updatedSubBranch = head $ branches updatedBranchNode
--                        updatedBranch = first (branchPrefix ++) updatedSubBranch
--                    in node {branches = less ++ updatedBranch:greater}
--                  else
--                    -- Otherwise we simply replace the branch with the updated one.
--                    let updatedBranch = (branchPrefix, updatedBranchNode)
--                    in node {branches = less ++ updatedBranch:greater}
--             else
--               node




key1 = [1,2,3]
key2 = [1,2,2]
s1 = tinsert key1 'a' empty
s2 = tinsert key2 'b' s1
