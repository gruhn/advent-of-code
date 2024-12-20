module Main (main) where
import Utils (withCoords, safeMinimumBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (guard)
import Data.Maybe (isJust, maybeToList, fromJust)
import Data.List (tails)
import Data.Containers.ListUtils (nubOrd)
import Data.Ord (comparing)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

type Pos = (Int, Int)

parse :: String -> (Map Pos Char, Pos, Pos)
parse input = (grid, start_pos, target_pos)
  where
    pos_char_pairs :: [(Pos, Char)]
    pos_char_pairs = withCoords $ lines input

    grid :: Map Pos Char
    grid = Map.fromList pos_char_pairs

    start_pos  = head [ pos | (pos, 'S') <- pos_char_pairs ]
    target_pos = head [ pos | (pos, 'E') <- pos_char_pairs ]

distantPositions :: Pos -> Int -> [Pos]
distantPositions (x,y) dist = do
  dx <- [-dist .. dist]
  dy <- nubOrd [-dist + abs dx, dist - abs dx]
  return (x+dx, y+dy)

dijkstra :: (Pos -> [Pos]) -> Pos -> Map Pos Int
dijkstra transitions start = go (IntMap.singleton 0 [start]) Map.empty
  where
    update_dist :: Int -> Pos -> Map Pos Int -> Map Pos Int
    update_dist dist pos = Map.insertWith min pos dist

    go :: IntMap [Pos] -> Map Pos Int -> Map Pos Int
    go queue dist_map =
      case IntMap.minViewWithKey queue of
        Nothing -> dist_map
        Just ((next_nodes_cost, next_nodes), queue_without_next) ->
          let
            new_dist_map :: Map Pos Int
            new_dist_map = foldr (update_dist next_nodes_cost) dist_map next_nodes 

            queue_additions :: IntMap [Pos]
            queue_additions = IntMap.map nubOrd $ IntMap.fromListWith (++) $ do
              pos      <- next_nodes
              neighbor <- transitions pos
              let new_neighbor_cost = next_nodes_cost + 1
              case Map.lookup neighbor new_dist_map of
                Nothing -> do 
                  return (new_neighbor_cost, [neighbor])
                Just old_neighbor_cost -> do
                  guard (new_neighbor_cost < old_neighbor_cost)
                  return (new_neighbor_cost, [neighbor])

            new_queue :: IntMap [Pos]
            new_queue = IntMap.unionWith (++) queue_without_next queue_additions
          in
            go new_queue new_dist_map

shortestPath :: Map Pos Int -> Pos -> Maybe [Pos]
shortestPath dist_map start_pos = 
  case Map.lookup start_pos dist_map of
    Nothing -> Nothing
    Just 0  -> Just [start_pos]
    Just _  -> do
      (next_pos, _) <- safeMinimumBy (comparing snd) $ do
        neighbor <- distantPositions start_pos 1
        dist     <- maybeToList $ Map.lookup neighbor dist_map
        return (neighbor, dist)
      rest_path <- shortestPath dist_map next_pos 
      return (start_pos : rest_path)

main :: IO ()
main = do
  (grid, start_pos, target_pos) <- parse <$> readFile "input/20.txt"

  let is_walkable :: Pos -> Bool
      is_walkable pos = isJust $ do
        tile <- Map.lookup pos grid
        guard $ tile /= '#'

      transitions :: Pos -> [Pos]
      transitions pos = do
        neighbor <- distantPositions pos 1
        guard $ is_walkable neighbor
        return neighbor

      distance_to_target :: Map Pos Int
      distance_to_target = dijkstra transitions target_pos

      no_cheat_path :: [Pos]
      no_cheat_path = fromJust $ shortestPath distance_to_target start_pos

      no_cheat_dist :: Int
      no_cheat_dist = length no_cheat_path - 1

      solution :: Int -> Int
      solution max_cheat_dist = length $ do
        (pos : poses_before) <- tails $ reverse no_cheat_path
        cheat_dist <- [2 .. max_cheat_dist]
        cheat_end  <- distantPositions pos cheat_dist
        guard $ is_walkable cheat_end
        dist <- maybeToList $ Map.lookup cheat_end distance_to_target
        let total_dist = length poses_before + dist + cheat_dist
        guard $ no_cheat_dist - total_dist >= 100

  putStr "Part 1: "
  print $ solution 2

  putStr "Part 2: "
  print $ solution 20
