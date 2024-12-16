module Main (main) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Utils (withCoords)
import Data.Foldable (find)
import Data.Maybe (fromJust)
import Algorithm.Search (dijkstraAssoc)
import Data.Containers.ListUtils (nubOrd)
import Control.Monad (guard)
import Data.Ord (comparing)

type Pos = (Int,Int)

parse :: String -> Map Pos Char
parse = Map.fromList . withCoords . lines

data Dir = North | West | South | East
  deriving (Eq, Ord, Show)

type State = (Dir, Pos)

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

next :: Map Pos Char -> State -> [(State, Int)]
next grid (dir, pos) = maybe_step ++ turns
  where
    turns :: [(State, Int)]
    turns =
      [ ((turnClockwise dir, pos), 1000)
      , ((turnCounterClockwise dir, pos), 1000)
      ]

    maybe_step :: [(State, Int)]
    maybe_step =
      let new_state = (dir, step dir pos) in
      case Map.lookup (snd new_state) grid of
        Just c | c /= '#' -> [(new_state, 1)]
        _ -> []

type Path = ([State], Int)

-- bestPaths :: (State -> [(State, Int)]) -> State -> [Path]
-- bestPaths transitions start = do
--   (next_state, cost) <- transitions start
--   (path, path_cost)  <- minimaBy (comparing snd) $ bestPaths transitions next_state
--   return (next_state : path, cost + path_cost)

-- dijkstra :: (State -> [(State, Int)]) -> State -> [(State, Int)]
-- dijkstra transitions start = go (Map.singleton 0 [start]) Map.empty
--   where
--     go :: Map Int [State] -> Map State Int -> [(State, Int)]
--     go worklist dist =
--       case Map.minViewWithKey worklist of
--         Nothing -> []
--         Just ((next_nodes_dist, next_nodes), worklist_without_next) ->
--           let
--             min_next :: [(State, Int)]
--             min_next = map (,next_nodes_dist) next_nodes

--             new_dist :: Map State Int
--             new_dist = dist <> Map.fromList min_next

--             worklist_additions :: Map Int [State]
--             worklist_additions = Map.map nubOrd . Map.fromListWith (<>) $ do
--               node             <- next_nodes
--               (neighbor, dist) <- transitions node
--               let new_neighbor_dist = next_nodes_dist + dist
--                   old_neighbor_dist = Map.findWithDefault maxBound neighbor new_dist
--               guard (new_neighbor_dist < old_neighbor_dist)
--               return (new_neighbor_dist, [neighbor])

--             new_worklist :: Map Int [State]
--             new_worklist = worklist_without_next <> worklist_additions
--           in
--             min_next ++ go new_worklist new_dist

main :: IO ()
main = do
  grid <- parse <$> readFile "input/16.txt"

  let start_pos  = fst $ fromJust $ find ((=='S') . snd) $ Map.toList grid
      target_pos = fst $ fromJust $ find ((=='E') . snd) $ Map.toList grid

      is_target :: State -> Bool
      is_target (_, pos) = pos == target_pos

  print $ dijkstraAssoc
    (next grid)
    is_target
    (East, start_pos)
