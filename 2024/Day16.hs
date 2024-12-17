module Main (main) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Utils (withCoords, minimaBy)
import Data.Foldable (find)
import Data.Maybe (fromJust)
import Data.Containers.ListUtils (nubOrd)
import Control.Monad (guard)
import Data.Ord (comparing)
import qualified Data.Set as Set

type Pos = (Int,Int)

data Dir = North | West | South | East
  deriving (Eq, Ord, Show)

type Node = (Dir, Pos)

step :: Dir -> Pos -> Pos
step dir (x,y) =
  case dir of
    North -> (x,y-1)
    South -> (x,y+1)
    West  -> (x-1,y)
    East  -> (x+1,y)

turnClockwise :: Dir -> Dir
turnClockwise dir =
  case dir of
    North -> East
    East  -> South
    South -> West
    West  -> North

turnCounterClockwise :: Dir -> Dir
turnCounterClockwise =
  turnClockwise . turnClockwise . turnClockwise

next :: Map Pos Char -> Node -> [(Node, Int)]
next grid (dir, pos) = maybe_step ++ turns
  where
    turns :: [(Node, Int)]
    turns =
      [ ((turnClockwise dir, pos), 1000)
      , ((turnCounterClockwise dir, pos), 1000)
      ]

    maybe_step :: [(Node, Int)]
    maybe_step =
      let new_state = (dir, step dir pos) in
      case Map.lookup (snd new_state) grid of
        Just c | c /= '#' -> [(new_state, 1)]
        _ -> []

dijkstra :: (Node -> [(Node, Int)]) -> Node -> [Map Node (Int, [Node])]
dijkstra transitions start = go (Map.singleton 0 [(start, start)]) Map.empty
  where
    go :: Map Int [(Node, Node)] -> Map Node (Int, [Node]) -> [Map Node (Int, [Node])]
    go worklist dist =
      case Map.minViewWithKey worklist of
        Nothing -> []
        Just ((next_nodes_dist, next_nodes), worklist_without_next) ->
          let
            f :: (Int, [Node]) -> (Int, [Node]) -> (Int, [Node])
            f (cost1, ns1) (cost2, ns2)
              | cost1 < cost2 = (cost1, ns1)
              | cost1 > cost2 = (cost2, ns2)
              | otherwise     = (cost1, ns1 ++ ns2)

            new_dist :: Map Node (Int, [Node])
            new_dist = Map.unionWith f dist $ Map.fromListWith f $ do 
              (src, trg) <- next_nodes
              return (trg, (next_nodes_dist, [src]))

            worklist_additions :: Map Int [(Node, Node)]
            worklist_additions = Map.map nubOrd 
              $ Map.fromListWith (++) $ do
              (_, node)             <- next_nodes
              (neighbor, step_dist) <- transitions node
              let new_neighbor_dist = next_nodes_dist + step_dist
              case Map.lookup neighbor new_dist of
                Nothing -> do 
                  return (new_neighbor_dist, [(node, neighbor)])
                Just (old_neighbor_dist, _) -> do
                  guard (new_neighbor_dist < old_neighbor_dist)
                  return (new_neighbor_dist, [(node, neighbor)])

            new_worklist :: Map Int [(Node, Node)]
            new_worklist = Map.unionWith (++) worklist_without_next worklist_additions
          in
            dist : go new_worklist new_dist

bestPaths :: Node -> Map Node (Int, [Node]) -> [[Node]]
bestPaths target dist_map = 
  case Map.lookup target dist_map of
    Nothing -> error "bestPath: Nothing"
    Just (_, pred_nodes) -> do
      pred_node <- pred_nodes
      if pred_node == target then
        return [target]
      else do
        path <- bestPaths pred_node dist_map
        return (target : path)

main :: IO ()
main = do
  grid <- Map.fromList . withCoords . lines <$> readFile "input/16.txt"

  let start_pos  = fst $ fromJust $ find ((=='S') . snd) $ Map.toList grid
      target_pos = fst $ fromJust $ find ((=='E') . snd) $ Map.toList grid

      dist_map = last $ dijkstra (next grid) (East, start_pos)

      targets = minimaBy (comparing snd) $ do 
        (node@(_, pos), (dist, _)) <- Map.toList dist_map
        guard $ pos == target_pos
        return (node, dist)

  putStr "Part 1: "
  print $ map snd targets 

  putStr "Part 2: "
  print $ Set.size $ Set.fromList $ do
    (target_node, _) <- targets
    path <- bestPaths target_node dist_map
    (_, pos) <- path
    return pos
