module Main (main) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Utils (withCoords)
import Data.Containers.ListUtils (nubOrd)
import Control.Monad (guard)
import qualified Data.Set as Set
import Data.Set (Set)

type Pos = (Int, Int)

data Dir = North | West | South | East
  deriving (Eq, Ord, Show)

type Node = (Dir, Pos)

step :: Node -> Node
step (dir, (x,y)) =
  case dir of
    North -> (dir, (x,y-1))
    South -> (dir, (x,y+1))
    West  -> (dir, (x-1,y))
    East  -> (dir, (x+1,y))

turnClockwise :: Node -> Node
turnClockwise (dir, pos) =
  case dir of
    North -> (East , pos)
    East  -> (South, pos)
    South -> (West , pos)
    West  -> (North, pos)

turnCounterClockwise :: Node -> Node
turnCounterClockwise =
  turnClockwise . turnClockwise . turnClockwise

data Backpointer = Back Int [Node]

join :: Backpointer -> Backpointer -> Backpointer
join (Back cost1 preds1) (Back cost2 preds2) 
  | cost1 < cost2 = Back cost1 preds1
  | cost1 > cost2 = Back cost2 preds2
  | otherwise     = Back cost1 (preds1 ++ preds2)

type PriorityQueue = Map Int [(Node, Node)]

dijkstra :: (Node -> [(Node, Int)]) -> Node -> Map Node Backpointer
dijkstra transitions start = go (Map.singleton 0 [(start, start)]) Map.empty
  where
    insert_backpointer :: Int -> (Node, Node) -> Map Node Backpointer -> Map Node Backpointer 
    insert_backpointer cost (source, target) = Map.insertWith join target (Back cost [source])

    go :: PriorityQueue -> Map Node Backpointer -> Map Node Backpointer
    go queue backpointers =
      case Map.minViewWithKey queue of
        Nothing -> backpointers
        Just ((next_nodes_cost, next_nodes), queue_without_next) ->
          let
            new_backpointers :: Map Node Backpointer
            new_backpointers = foldr (insert_backpointer next_nodes_cost) backpointers next_nodes 

            queue_additions :: PriorityQueue
            queue_additions = Map.map nubOrd $ Map.fromListWith (++) $ do
              (_, node)             <- next_nodes
              (neighbor, step_cost) <- transitions node
              let new_neighbor_cost = next_nodes_cost + step_cost
              case Map.lookup neighbor new_backpointers of
                Nothing -> do 
                  return (new_neighbor_cost, [(node, neighbor)])
                Just (Back old_neighbor_cost _) -> do
                  guard (new_neighbor_cost <= old_neighbor_cost)
                  return (new_neighbor_cost, [(node, neighbor)])

            new_queue :: PriorityQueue
            new_queue = Map.unionWith (++) queue_without_next queue_additions
          in
            go new_queue new_backpointers

bestPaths :: Node -> Map Node Backpointer -> [[Node]]
bestPaths target dist_map = 
  case Map.lookup target dist_map of
    Nothing -> error "bestPath: Nothing"
    Just (Back _ pred_nodes) -> do
      pred_node <- pred_nodes
      if pred_node == target then
        return [target]
      else do
        path <- bestPaths pred_node dist_map
        return (target : path)

parse :: String -> (Set Pos, Pos, Pos)
parse input = (walkable_poses, start_pos, target_pos)
  where
    pos_char_pairs :: [(Pos, Char)]
    pos_char_pairs = withCoords $ lines input

    start_pos = head [ pos | (pos, 'S') <- pos_char_pairs ]
    target_pos = head [ pos | (pos, 'E') <- pos_char_pairs ]
    walkable_poses = Set.fromList [ pos | (pos, c) <- pos_char_pairs, c /= '#' ]

main :: IO ()
main = do
  (walkable_poses, start_pos, target_pos) <- parse <$> readFile "input/16.txt"

  let transitions :: Node -> [(Node, Int)]
      transitions node =
        let
          turns :: [(Node, Int)]
          turns = [(turnClockwise node, 1000), (turnCounterClockwise node, 1000)]

          maybe_step :: [(Node, Int)]
          maybe_step = do
            let next_node = step node
            guard $ Set.member (snd next_node) walkable_poses
            return (next_node, 1)
        in
          maybe_step ++ turns
  
      backpointers :: Map Node Backpointer
      backpointers = dijkstra transitions (East, start_pos)

  putStr "Part 1: "
  print $ minimum $ do
    ((_, pos), Back cost _) <- Map.toList backpointers
    guard $ pos == target_pos
    return cost

  putStr "Part 2: "
  print $ Set.size $ Set.fromList $ do
    (dir, pos) <- Map.keys backpointers
    guard $ pos == target_pos 
    path <- bestPaths (dir, pos) backpointers
    map snd path
