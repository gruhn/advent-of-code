module PairingHeap 
  ( Heap(Empty)
  , empty
  , isEmpty
  , merge
  , insert
  , findMax
  , deleteMax
  , viewMax
  , fromList
  , fromListWith
  , toList
  ) where

data Heap k v = Empty | Node k v [Heap k v]

empty :: Heap k v
empty = Empty

isEmpty :: Heap k v -> Bool
isEmpty Empty = True
isEmpty _     = False

insert :: Ord k => k -> v -> Heap k v -> Heap k v
insert key val heap = merge (Node key val []) heap

merge :: Ord k => Heap k v -> Heap k v -> Heap k v
merge Empty heap2 = heap2
merge heap1 Empty = heap1
merge heap1@(Node k1 v1 cs1) heap2@(Node k2 v2 cs2)
  | k1 >= k2  = Node k1 v1 (heap2 : cs1)
  | otherwise = Node k2 v2 (heap1 : cs2)

mergePairs :: Ord k => [Heap k v] -> Heap k v
mergePairs []         = Empty
mergePairs [h]        = h
mergePairs (h1:h2:hs) = merge (merge h1 h2) (mergePairs hs)

deleteMax :: Ord k => Heap k v -> Heap k v
deleteMax Empty         = Empty
deleteMax (Node _ _ cs) = mergePairs cs

findMax :: Heap k v -> Maybe v
findMax Empty            = Nothing
findMax (Node _ value _) = Just value

viewMax :: Ord k => Heap k v -> Maybe (v, Heap k v)
viewMax heap = do
  val <- findMax heap
  return (val, deleteMax heap)

fromList :: Ord k => [(k,v)] -> Heap k v
fromList = foldr (uncurry insert) Empty

toList :: Heap k v -> [(k,v)]
toList Empty         = []
toList (Node k v cs) = (k,v) : concatMap toList cs

fromListWith :: Ord k => (v -> k) -> [v] -> Heap k v
fromListWith f = foldr (\v -> insert (f v) v) Empty
