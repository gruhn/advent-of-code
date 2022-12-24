module Main where

import Utils (withCoordinates, takeDistinct)
import Algorithm.Search (dijkstra, aStar)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Foldable ( traverse_, foldl' )
import Data.Maybe (fromJust)
import Data.List ( foldl' )
import qualified Data.Vector as V

type Vec = (Int,Int)

parse :: String -> (Int, Int, [(Vec, Vec)])
parse str = (width, height, pos_dir_pairs)
  where
    width = length (head $ lines str) - 2
    height = length (lines str) - 2

    pos_dir_pairs = do
      ((x,y), char) <- withCoordinates (lines str)
      let pos = (x-1, y-1)
      case char of
        '>' -> return (pos, (1,0))
        '<' -> return (pos, (-1,0))
        '^' -> return (pos, (0,-1))
        'v' -> return (pos, (0,1))
        _   -> []

stormStates :: (Int, Int, [(Vec, Vec)]) -> V.Vector (Map Vec (Set Vec))
stormStates (width, height, pos_dir_pairs) = states
  where
    initial_state :: Map Vec (Set Vec)
    initial_state = M.fromListWith (<>) $ do
      (pos, dir) <- pos_dir_pairs
      return (dir, S.singleton pos)

    move :: Vec -> Vec -> Vec
    move (dx,dy) (x,y) = (new_x, new_y)
      where
        new_x = (x+dx) `mod` width
        new_y = (y+dy) `mod` height
    
    next_state :: Map Vec (Set Vec) -> Map Vec (Set Vec)
    next_state = M.mapWithKey (S.map . move)

    states :: V.Vector (Map Vec (Set Vec))
    states = V.fromList
      $ takeDistinct 
      $ iterate next_state initial_state


data State = State { getTime :: Int, getPos :: Vec } 
  deriving (Eq, Ord, Show)

neighbors :: Vec -> Set Vec
neighbors (x,y) = S.fromList 
  [ (x,y), (x+1,y), (x-1,y), (x,y+1), (x,y-1) ]

nextStates :: (Int -> Map Vec (Set Vec)) -> Int -> Int -> State -> Set State
nextStates storm_state_at width height (State time pos) = S.map (State $ time+1) next_pos_options
  where
    in_bounds (x,y) =
         (x,y) == (0,-1)
      || (x,y) == (width-1,height)
      || (0 <= x && x < width && 0 <= y && y < height)

    next_pos_options = S.filter in_bounds
      $ foldl' (S.\\) (neighbors pos) 
      $ storm_state_at (time+1)

manhattanDist :: Vec -> Vec -> Int
manhattanDist (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)

shortestPath :: (State -> Set State) -> State -> Vec -> Maybe (Int, [State])
shortestPath next_states start_state goal_pos = 
  let 
    step_cost _ _ = 1
    cost_lower_bound state = manhattanDist (getPos state) goal_pos
    is_goal_state = (goal_pos ==) . getPos
  in 
    aStar 
      next_states 
      step_cost 
      cost_lower_bound 
      is_goal_state 
      start_state

main :: IO ()
main = do
  input <- parse <$> readFile "input/24.txt"

  let (width, height, _) = input
    
      start_pos = (0, -1)
      goal_pos = (width-1, height)

      storm_states = stormStates input
      storm_state_at time = storm_states V.! (time `mod` length storm_states)

      next_states = nextStates storm_state_at width height

      start_state1 = State 0 start_pos
      (cost1, path1) = fromJust $ shortestPath next_states start_state1 goal_pos
      start_state2 = last path1
      (cost2, path2) = fromJust $ shortestPath next_states start_state2 start_pos
      start_state3 = last path2
      (cost3, path3) = fromJust $ shortestPath next_states start_state3 goal_pos

  putStr "Part 1: "
  print cost1

  putStr "Part 2: "
  print $ sum [cost1, cost2, cost3]

