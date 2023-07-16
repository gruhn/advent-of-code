{-# LANGUAGE ScopedTypeVariables #-}
module PrefixTree where

data PrefixTree a = PrefixTree
  { isWordEnd :: Bool 
  , getChildren :: Map a (PrefixTree a)
  } deriving Show

empty :: PrefixTree a
empty = PrefixTree False M.empty

insert :: Ord a => [a] -> PrefixTree a -> PrefixTree a
insert [] tree = tree { isWordEnd = True }
insert (a:as) (PrefixTree is_word_end children) =
  PrefixTree is_word_end 
    $ M.adjust (insert as) a
    $ M.insertWith (flip const) a empty children

member :: Ord a => [a] -> PrefixTree a -> Bool
member [] tree = isWordEnd tree
member (a:as) tree = 
  case M.lookup a (getChildren tree) of
    Nothing       -> False
    Just sub_tree -> member as sub_tree

toList :: PrefixTree a -> [[a]]
toList (PrefixTree is_word_end children) =
  (if is_word_end then ([] :) else id) $ do
    (a, tree) <- M.toList children
    as <- toList tree
    return (a:as)

fromList :: Ord a => [[a]] -> PrefixTree a
fromList = foldr insert empty
