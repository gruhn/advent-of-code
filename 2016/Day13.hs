{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Control.Monad (guard)
import Data.Bits (popCount)

distinct :: Ord a => [a] -> [a]
distinct = S.toList . S.fromList

puzzleInput :: (Int,Int) -> Int
puzzleInput (x,y) = x*x + 3*x + 2*x*y + y + y*y + 1350

nextStates :: (Int,Int) -> [(Int,Int)]
nextStates (x,y) = filter is_valid neighbors
  where
    neighbors = [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]

    is_open_space = even . popCount . puzzleInput
    is_in_bounds (x',y') = 0 <= x' && 0 <= y'
    is_valid xy = is_open_space xy && is_in_bounds xy

dijkstra :: forall a. Ord a => (a -> [a]) -> (a -> a -> Int) -> a -> [(a, Int)]
dijkstra neighbors_of dist_between start = go (M.singleton 0 [start]) mempty
  where
    go :: M.Map Int [a] -> M.Map a Int -> [(a, Int)]
    go worklist dist 
      | M.null worklist = [] 
      | otherwise       = min_next ++ go new_worklist new_dist
      where
        ((next_nodes_dist, next_nodes), worklist_without_next) = M.deleteFindMin worklist

        min_next = zip next_nodes (repeat next_nodes_dist)
        new_dist = dist <> M.fromList min_next

        worklist_additions = M.map distinct . M.fromListWith (<>) $ do
          node     <- next_nodes
          neighbor <- neighbors_of node
          let new_neighbor_dist = next_nodes_dist + dist_between node neighbor
              old_neighbor_dist = M.findWithDefault maxBound neighbor new_dist
          guard (new_neighbor_dist < old_neighbor_dist)
          return (new_neighbor_dist, [neighbor])

        new_worklist = worklist_without_next <> worklist_additions

main :: IO ()
main = do
  let start = (1,1)
      transition_cost _ _ = 1
      shortest_distances = dijkstra nextStates transition_cost (1,1)

  putStr "Part 1: "
  let target = (31, 39)
  print $ L.find ((target ==) . fst) shortest_distances

  putStr "Part 2: "
  print $ length $ takeWhile ((<= 50) . snd) shortest_distances