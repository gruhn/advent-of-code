module Main where

import Data.Foldable (find)
import qualified Data.Map as M
import Data.List (mapAccumL)

type Point = (Int,Int)

manhattanDist :: Point -> Int
manhattanDist (x, y) = abs x + abs y

indexToPoint :: Int -> Point
indexToPoint index_one_based
  | index_one_based <= 0 = error "not defined for integers <= 0"
  | index_one_based == 1 = (0,0)
  | otherwise            = (x,y)
  where
    -- make index zero based to simplify calculations
    index = index_one_based - 1

    -- distance from center to the "spiral shell" where `index` is located
    radius = ceiling $ (sqrt (fromIntegral index + 1) - 1) / 2

    outer_shell_side_length = 2 * radius     + 1
    inner_shell_side_length = 2 * (radius-1) + 1

    -- compute the index relativ to just the shell 
    inner_shell_area = inner_shell_side_length^2
    index_on_shell = index - inner_shell_area

    -- determinte on which "edge" of the shell the index is located, i.e. 
    -- left, top, right, or bottom edge. The edges are considered forward 
    -- shifted, so 
    --   * the top-right corner is part of the right edge
    --   * top-left corner is part of the top edge 
    --   * the bottom-left corner is part of the left edge and
    --   * the bottom-right corner is part of the bottom edge
    outer_shell_edge = index_on_shell `div` (outer_shell_side_length-1)

    -- compute the index relative to just the specific edge of the shell
    index_on_edge = index_on_shell `mod` (outer_shell_side_length-1)

    -- `index_on_edge` points from the rim inwards, e.g. on the top 
    -- edge of the shell it points from the right end to the left. But to get
    -- a coordinate out of it, we must adjust it so it points from the center
    -- outward (either left or right), since the coordinate origin is in the 
    -- center of the edge.
    index_on_edge_from_origin = index_on_edge - radius + 1

    (x,y) = 
      case outer_shell_edge of
        0 -> (radius, index_on_edge_from_origin)    -- right edge
        1 -> (-index_on_edge_from_origin, radius)   -- top edge
        2 -> (-radius, -index_on_edge_from_origin)  -- left edge
        3 -> (index_on_edge_from_origin, -radius)   -- bottom edge
        _ -> undefined

neighborsOf :: Point -> [Point]
neighborsOf (x,y) = 
  [ (x-1,y+1), (x,y+1), (x+1,y+1)
  , (x-1,y)  ,          (x+1,y) 
  , (x-1,y-1), (x,y-1), (x+1,y-1) ]

type ValueTable = M.Map Point Int

valueAt :: ValueTable -> Point -> (ValueTable, Int)
valueAt value_table (0,0) = (M.insert (0,0) 1 value_table, 1)
valueAt value_table point = (M.insert point value value_table, value)
  where
    value = sum [ M.findWithDefault 0 neighbor value_table | neighbor <- neighborsOf point ]

main :: IO ()
main = do 
  let input = 368078

  putStr "Part 1: "
  print 
    $ manhattanDist 
    $ indexToPoint input

  putStr "Part 2: "
  print 
    $ find (> input) 
    $ snd 
    $ mapAccumL valueAt M.empty
    $ map indexToPoint [1..]
