module Main where
import qualified Data.List as L
import Data.Bits (popCount)
import Dijkstra (dijkstra)

puzzleInput :: (Int,Int) -> Int
puzzleInput (x,y) = x*x + 3*x + 2*x*y + y + y*y + 1350

nextStates :: (Int,Int) -> [(Int,Int)]
nextStates (x,y) = filter is_valid neighbors
  where
    neighbors = [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]

    is_open_space = even . popCount . puzzleInput
    is_in_bounds (x',y') = 0 <= x' && 0 <= y'
    is_valid xy = is_open_space xy && is_in_bounds xy

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