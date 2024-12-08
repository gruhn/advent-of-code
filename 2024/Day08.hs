module Main (main) where

import Utils (withCoords)
import Control.Monad (guard)
import Data.Containers.ListUtils (nubOrd)
import Data.List (tails)

type Pos = (Int, Int)

sameFrequencyPairs :: [(Pos, Char)] -> [(Pos, Pos)]
sameFrequencyPairs frequencies = do
  (pos1, freq1) : rest <- tails frequencies
  (pos2, freq2) <- rest
  guard (freq1 == freq2)
  return (pos1, pos2)

lineSegment :: Pos -> Pos -> [Pos]
lineSegment (x1,y1) (x2,y2) = (x2,y2) : lineSegment (x2,y2) (2*x2-x1, 2*y2-y1)
  
main :: IO ()
main = do
  input <- withCoords . lines <$> readFile "input/08.txt"

  let (max_x, max_y) = maximum (map fst input)

      in_bounds :: Pos -> Bool
      in_bounds (x,y) = 0 <= x && x <= max_x && 0 <= y && y <= max_y

      frequencies :: [(Pos, Char)]
      frequencies = filter ((/= '.') . snd) input

  putStr "Part 1: "
  print $ length $ nubOrd $ do
    (pos1, pos2) <- sameFrequencyPairs frequencies
    let antinode1 = lineSegment pos1 pos2 !! 1
    let antinode2 = lineSegment pos2 pos1 !! 1
    filter in_bounds [antinode1, antinode2]

  putStr "Part 2: "
  print $ length $ nubOrd $ do
    (pos1, pos2) <- sameFrequencyPairs frequencies
    let anitnodes1 = takeWhile in_bounds $ lineSegment pos1 pos2
    let anitnodes2 = takeWhile in_bounds $ lineSegment pos2 pos1
    anitnodes1 ++ anitnodes2 
