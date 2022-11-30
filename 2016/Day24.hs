module Main where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
import Control.Monad (guard, zipWithM)
import Data.Char (digitToInt, intToDigit)
import Algorithm.Search (aStar)
import Data.Foldable (foldl', for_, foldlM)
import Data.Maybe (catMaybes, mapMaybe, maybeToList)
import Control.Arrow (second)
import Data.Function (on)
import Data.List (minimumBy)

type Point = (Int, Int)

points2D :: [[Point]]
points2D = [ [ (x,y) | x <- [0..] ] | y <- [0..] ]

zip2D :: [[a]] -> [[b]] -> [[(a,b)]]
zip2D = zipWith zip

symmetricPairs :: Ord a => [a] -> [(a,a)]
symmetricPairs = go . L.sort
  where
    go []     = []
    go (a:as) = [ (a,a') | a' <- as ] <> go as

data Tile = Wall | Open | Start | Goal Int
  deriving Eq

instance Show Tile where
  show Wall     = "#"
  show Open     = " "
  show Start    = "0"
  show (Goal g) = show g

toTile :: Char -> Tile
toTile '.' = Open
toTile '#' = Wall
toTile '0' = Start
toTile  g  = Goal (digitToInt g)

tileMap :: String -> M.Map Point Tile
tileMap str = M.fromList tiles_with_coordinates
  where
    tiles = fmap toTile <$> lines str
    tiles_with_coordinates =
      filter (reachable . snd) $ concat $ zip2D points2D tiles

    reachable Wall = False
    reachable _    = True

goalTiles :: M.Map Point Tile -> M.Map Point Int
goalTiles = M.map get_goal . M.filter is_goal_tile
  where
    get_goal (Goal g) = g
    get_goal _        = undefined

    is_goal_tile (Goal _) = True
    is_goal_tile _        = False

neighbors :: M.Map Point Tile -> Point -> [Point]
neighbors tiles (x,y) = L.filter (`M.member` tiles) all_neighbors
  where
    all_neighbors = [ (x+1,y), (x-1,y), (x,y+1), (x,y-1) ]

manhattanDistance :: Point -> Point -> Int
manhattanDistance (x1,y1) (x2,y2) =
  abs (x1 - x2) + abs (y1 - y2)

shortestPath :: M.Map Point Tile -> Point -> Point -> Maybe [Point]
shortestPath tiles start goal = snd <$> aStar
  (neighbors tiles)         -- expand current position
  (\_ _ -> 1)               -- transition cost
  (manhattanDistance goal)  -- remaining cost lower bound 
  (== goal)                 -- goal checking predicate
  start                     -- start position

allShortestPaths :: M.Map Point Tile -> [Point] -> M.Map (Point, Point) [Point]
allShortestPaths tiles points =
    M.unions $ do
      (p1, p2) <- symmetricPairs points
      path     <- maybeToList $ shortestPath tiles p1 p2
      let forward  = M.singleton (p1,p2) path
          backward = M.singleton (p2,p1) (tail (L.reverse path) <> [p1])
      [forward, backward]

route :: (Point -> Point -> Maybe [Point]) -> [Point] -> Maybe [Point]
route points_between [] = Just []
route points_between (start:rest) = (start :) <$> go start rest
  where
    go :: Point -> [Point] -> Maybe [Point]
    go p1 []      = Just []
    go p1 (p2:ps) = do 
      path <- points_between p1 p2 
      rest <- go p2 ps
      return $ path <> rest

showPath :: M.Map Point Tile -> [Point] -> String
showPath open_tiles path = unlines (concatMap show_tile_at <$> points2D')
  where
    (xs, ys) = unzip (M.keys open_tiles)
    width  = maximum xs + 2
    height = maximum ys + 2

    points2D' = take width <$> take height points2D

    path_layer = M.fromList $ zip path $ repeat "."
    open_layer = M.map show open_tiles
    goal_layer = M.map show (goalTiles open_tiles)

    layers = M.unions [goal_layer, path_layer, open_layer]

    show_tile_at point = M.findWithDefault "#" point layers

main :: IO ()
main = do
  tiles <- tileMap <$> readFile "input/24.txt"

  let start = head . M.keys . M.filter (Start ==) $ tiles
      goals = M.keys $ goalTiles tiles

      all_shortest_paths = allShortestPaths tiles (start:goals)
      interpolate p1 p2 = M.lookup (p1, p2) all_shortest_paths

      all_routes = mapMaybe (route interpolate) [ [start] <> perm            | perm <- L.permutations goals ]
      all_tours  = mapMaybe (route interpolate) [ [start] <> perm <> [start] | perm <- L.permutations goals ]

  putStr "Part 1: "
  let best_route = minimumBy (compare `on` length) all_routes
  print $ length best_route - 1
  putStr $ showPath tiles best_route

  putStr "Part 2: "
  let best_tour = minimumBy (compare `on` length) all_tours
  print $ length best_tour - 1
  putStr $ showPath tiles best_tour