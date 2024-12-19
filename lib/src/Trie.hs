module Trie
  ( Trie
  , singleton
  , empty
  , insert
  , union
  , fromList
  , toList
  , subTrie
  , member
  , stripPrefixes
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (lookup)
import Data.Maybe (maybeToList, isJust)
import Control.Monad (guard)

data Trie a = Node
  { isMember :: Bool
  , children :: Map a (Trie a)
  }

empty :: Trie a
empty = Node False Map.empty

singleton :: [a] -> Trie a
singleton []     = Node True  Map.empty
singleton (a:as) = Node False (Map.singleton a (singleton as))

union :: Ord a => Trie a -> Trie a -> Trie a
union node1 node2 = Node
  (node1.isMember || node2.isMember)
  (Map.unionWith union node1.children node2.children)

insert :: Ord a => [a] -> Trie a -> Trie a
insert as trie = trie `union` singleton as

-- difference :: Trie a -> Trie a -> Trie a
-- difference node1 node2 = Node
--   (ndoe1.isMember && not node2.isMember)
--   (Map.filter (== empty) $ Map.differenceWith  node1.children node2.children)

-- delete :: Ord a => [a] -> Trie a -> Trie a 
-- delete as trie = trie `difference` singleton as

fromList :: Ord a => [[a]] -> Trie a
fromList = foldr insert empty

-- | INTERNAL:
childrenToList :: Trie a -> [[a]]
childrenToList node = do
  (a, child) <- Map.toList node.children
  as <- toList child
  return (a:as)

toList :: forall a. Trie a -> [[a]]
toList node
  | node.isMember = [] : childrenToList node
  | otherwise     = childrenToList node

member :: Ord a => [a] -> Trie a -> Bool
member as trie = isJust $ do 
  sub_trie <- subTrie as trie
  guard sub_trie.isMember

subTrie :: Ord a => [a] -> Trie a -> Maybe (Trie a)
subTrie []     trie = Just trie
subTrie (a:as) trie = do
  child <- Map.lookup a trie.children
  subTrie as child

-- | INTERNAL:
stripPrefixesChildren :: Ord a => Trie a -> [a] -> [[a]]
stripPrefixesChildren _    []     = []
stripPrefixesChildren trie (a:as) = do
  child <- maybeToList $ subTrie [a] trie
  stripPrefixes child as

stripPrefixes :: Ord a => Trie a -> [a] -> [[a]]
stripPrefixes trie string
  | trie.isMember = string : stripPrefixesChildren trie string
  | otherwise     = stripPrefixesChildren trie string
