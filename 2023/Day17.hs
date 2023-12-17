module Main where
import Data.Map (Map)
import qualified Data.Map as Map
import Utils (withCoords)
import Data.Char (digitToInt)
import Algorithm.Search (dijkstra)
import Control.Monad (guard)

type Pos = (Int,Int)

parse :: String -> Map Pos Int
parse = Map.fromList . withCoords . map (map digitToInt) . lines

data Dir = Up | Dwn | Lft | Rgt
  deriving (Eq, Ord, Show)

type State = (Pos, [Dir])

move :: Pos -> Int -> Dir -> Pos
move (x,y) steps = \case
  Up  -> (x,y-steps)
  Dwn -> (x,y+steps)
  Lft -> (x-steps,y)
  Rgt -> (x+steps,y)

turnOptions :: Dir -> [Dir]
turnOptions = \case
  Up  -> [Lft,Rgt]
  Dwn -> [Lft,Rgt]
  Lft -> [Up,Dwn]
  Rgt -> [Up,Dwn]

path :: Pos -> Pos -> [Pos]
path (x1,y1) (x2,y2)
  | x1 == x2 && y1 == y2 = []
  | otherwise = pos : path pos (x2,y2)
  where pos = (x1 + signum (x2-x1), y1 + signum (y2-y1))

shortestPath :: Map Pos Int -> [Int] -> Maybe Int
shortestPath grid step_range = 
  let goal :: Pos
      goal = fst $ Map.findMax grid

      start :: State
      start = ((0,0), [Rgt,Dwn])

      is_goal :: State -> Bool
      is_goal (pos, _) = pos == goal

      next :: State -> [State]
      next ((x,y), dir_options) = do 
        steps <- step_range
        dir   <- dir_options
        let new_pos = move (x,y) steps dir
        guard $ new_pos `Map.member` grid
        return (new_pos, turnOptions dir)

      cost :: State -> State -> Int
      cost (from, _) (to, _) = 
        sum $ map (grid Map.!) $ path from to

   in fst <$> dijkstra next cost is_goal start

main :: IO ()
main = do
  grid <- parse <$> readFile "input/17.txt"

  putStr "Part 1: "
  print $ shortestPath grid [1..3]

  putStr "Part 2: "
  print $ shortestPath grid [4..10]
