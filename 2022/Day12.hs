module Main where
import Data.Map (Map)
import qualified Data.Map as M
import Data.Char (digitToInt)
import Algorithm.Search (dijkstra)
import GHC.Base (ord)
import Data.Maybe ( fromMaybe )
import Control.Applicative (Applicative(liftA2))

type Pos = (Int,Int)

type HeatMap = Map Pos Int

parse :: String -> (Pos, Pos, HeatMap)
parse str = (start_pos, target_pos, heat_map)
  where
    heat_map0 = M.fromList $ do
      (x, row) <- zip [0..] $ zip [0..] <$> lines str
      (y, cell) <- row
      return ((x,y), cell)

    start_pos = fst . head $ M.toList $ M.filter (=='S') heat_map0
    target_pos = fst . head $ M.toList $ M.filter (=='E') heat_map0

    elevation :: Char -> Int
    elevation 'S' = ord 'a'
    elevation 'E' = ord 'z'
    elevation char = ord char

    heat_map = M.map elevation heat_map0

neighbors :: HeatMap -> Pos -> [Pos]
neighbors heat_map (x,y) =
  filter (`M.member` heat_map) [ (x+1,y), (x-1, y), (x,y+1), (x,y-1) ]

elevationDiff :: HeatMap -> Pos -> Pos -> Maybe Int
elevationDiff heat_map pos1 pos2 =
  liftA2 (-) (M.lookup pos1 heat_map) (M.lookup pos2 heat_map)

main :: IO ()
main = do
  (start_pos, target_pos, heat_map) <- parse <$> readFile "input/12.txt"

  let step_cost _ _ = 1

  putStr "Part 1: "
  let at_most_one_higher pos1 pos2 =
        maybe False (>= -1) (elevationDiff heat_map pos1 pos2)

  print $ fst <$> dijkstra
    (\pos -> filter (pos `at_most_one_higher`) (neighbors heat_map pos))
    step_cost
    (==target_pos)
    start_pos

  putStr "Part 2: "
  let has_min_elevation pos = fromMaybe False $ do
        elevation <- M.lookup pos heat_map 
        return $ elevation == ord 'a'

  print $ fst <$> dijkstra
    (\pos -> filter (`at_most_one_higher` pos) (neighbors heat_map pos))
    step_cost
    has_min_elevation
    target_pos