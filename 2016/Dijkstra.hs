{-# LANGUAGE ScopedTypeVariables #-}
module Dijkstra (dijkstra) where

import Control.Monad (guard)
import qualified Data.Map as M
import qualified Data.Set as S

distinct :: Ord a => [a] -> [a]
distinct = S.toList . S.fromList

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
