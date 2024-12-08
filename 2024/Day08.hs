module Main (main) where

import Utils (withCoords)
import Control.Monad (guard)
import Data.Containers.ListUtils (nubOrd)
import Data.List (tails)

type Pos = (Int, Int)

lineSegment :: Pos -> Pos -> [Pos]
lineSegment (x1,y1) (x2,y2) = (x2,y2) : lineSegment (x2,y2) (2*x2-x1, 2*y2-y1)
  
main :: IO ()
main = do
  input <- withCoords . lines <$> readFile "input/08.txt"

  let (max_x, max_y) = maximum (map fst input)

      in_bounds :: Pos -> Bool
      in_bounds (x,y) = 0 <= x && x <= max_x && 0 <= y && y <= max_y

      antennas :: [(Pos, Char)]
      antennas = filter ((/= '.') . snd) input

      same_frequency_pairs :: [(Pos, Pos)]
      same_frequency_pairs = do
        (pos1, freq1) : rest <- tails antennas
        (pos2, freq2) <- rest
        guard $ freq1 == freq2
        return (pos1, pos2)

  putStr "Part 1: "
  print $ length $ nubOrd $ do
    (pos1, pos2) <- same_frequency_pairs
    let anti_node1 = lineSegment pos1 pos2 !! 1
    let anti_node2 = lineSegment pos2 pos1 !! 1
    filter in_bounds [anti_node1, anti_node2]

  putStr "Part 2: "
  print $ length $ nubOrd $ do
    (pos1, pos2) <- same_frequency_pairs
    let anti_nodes1 = takeWhile in_bounds $ lineSegment pos1 pos2
    let anti_nodes2 = takeWhile in_bounds $ lineSegment pos2 pos1
    anti_nodes1 ++ anti_nodes2
