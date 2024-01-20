module Main where

import Utils (withCoords, showGrid2D, assertM)
import Control.Monad (guard)
import qualified Data.HashSet as Set
import Data.Foldable (for_, traverse_)
import Data.Ratio ((%))

type Set = Set.HashSet

type Pos = (Int,Int)

data Grid = Grid
  { getPoses    :: Set Pos
  , getDim      :: Int
  , getStartPos :: Pos
  }

showGrid :: Grid -> Set Pos -> String
showGrid (Grid poses dim start_pos) visited = 
  let
    show_cell :: Int -> Int -> Char
    show_cell x y
      | (x,y) == start_pos       = 'S'
      | Set.member (x,y) visited = 'O'
      | Set.member (x,y) poses   = '.'
      | otherwise                = '#'
  in
    showGrid2D show_cell dim dim

parseGrid :: String -> Grid
parseGrid input = 
  let
    rows :: [String]
    rows = lines input

    coords :: [(Pos, Char)]
    coords = withCoords $ lines input

    start_pos :: Pos
    start_pos = head [ pos | (pos, 'S') <- coords ]

    pos_set :: Set Pos
    pos_set = Set.fromList $ do 
      (pos, tile) <- coords
      guard $ tile /= '#'
      return pos
  in
    Grid pos_set (length rows) start_pos

dist :: Pos -> Pos -> Int
dist (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

isRock :: Grid -> Pos -> Bool
isRock (Grid poses dim _) (x,y) =
  not $ Set.member (x `mod` dim, y `mod` dim) poses

neighbors :: Grid -> Pos -> [Pos]
neighbors grid (x,y) = do
  pos <- [ (x+1,y), (x-1,y), (x,y+1), (x,y-1) ]
  guard $ not $ isRock grid pos
  return pos

reachable :: Grid -> [Set Pos]
reachable grid =
  let 
    start :: Set Pos
    start = Set.singleton $ getStartPos grid

    expand_even :: (Set Pos, Set Pos, Set Pos) -> [Set Pos]
    expand_even (visited_even, visited_odd, frontier) = 
      let
        new_frontier :: Set Pos
        new_frontier = Set.fromList $ do
          pos <- Set.toList frontier
          neigh <- neighbors grid pos
          guard $ not $ Set.member neigh visited_even
          return neigh
      in
        visited_odd : expand_odd (visited_even <> new_frontier, visited_odd, new_frontier)

    expand_odd :: (Set Pos, Set Pos, Set Pos) -> [Set Pos]
    expand_odd (visited_even, visited_odd, frontier) = 
      let
        new_frontier :: Set Pos
        new_frontier = Set.fromList $ do
          pos <- Set.toList frontier
          neigh <- neighbors grid pos
          guard $ not $ Set.member neigh visited_odd
          return neigh
      in
        visited_even : expand_even (visited_even, visited_odd <> new_frontier, new_frontier)
  in
    expand_odd (start, Set.empty, start)

main :: IO ()
main = do
  grid <- parseGrid <$> readFile "input/21.txt"

  let center_diamond_64 = reachable grid !! 64
  putStr $ showGrid grid center_diamond_64

  putStr "Part 1: "
  print $ Set.size center_diamond_64

  putStrLn "Part 2: "
  assertM $ getDim grid == 131
  for_ [ 131*i + 65 | i <- [0,1,2] ] $ \steps -> do
    print (steps, Set.size $ reachable grid !! steps)
  -- manually interpolated polynomial:
  let poly t = 14881 % 17161 * t^2 + 22741 % 17161 * t - 133928 % 17161
  print $ poly 26501365
