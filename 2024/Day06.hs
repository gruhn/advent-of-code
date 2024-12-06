module Main (main) where

import Utils (withCoords)
import qualified Data.Set as Set
import Control.Monad (guard)
import Data.Set (Set)
import Data.Containers.ListUtils (nubOrd)
import Data.List (delete)

type Pos = (Int, Int)

type Config = (Pos, Dir)

data Grid = Grid
  { obstacles :: Set Pos
  , max_x     :: Int
  , max_y     :: Int
  }

parse :: String -> (Config, Grid)
parse input = (start_config, Grid obstacles max_x max_y)
  where
    pos_char_pairs :: [(Pos, Char)]
    pos_char_pairs = withCoords $ lines input

    start_config :: Config
    start_config = head $ do
      (pos, char) <- pos_char_pairs
      case char of
        '^' -> return (pos, Up )
        'v' -> return (pos, Dwn)
        '>' -> return (pos, Lft)
        '<' -> return (pos, Rgt)
        _   -> []

    obstacles :: Set Pos
    obstacles = Set.fromList [ pos | (pos, '#') <- pos_char_pairs ]

    (max_x, max_y) = maximum $ map fst pos_char_pairs

data Dir = Lft | Rgt | Up | Dwn
  deriving (Show, Eq, Ord)

turnRight :: Dir -> Dir
turnRight dir =
  case dir of
    Lft -> Up
    Rgt -> Dwn
    Up  -> Rgt
    Dwn -> Lft

turnAround :: Dir -> Dir
turnAround = turnRight . turnRight

step :: Dir -> Pos -> Pos
step dir (x,y) =
  case dir of
    Lft -> (x-1,y)
    Rgt -> (x+1,y)
    Up  -> (x,y-1)
    Dwn -> (x,y+1)

stepBack :: Dir -> Pos -> Pos
stepBack dir = step (turnAround dir)

nextObstacle :: Grid -> Config -> Maybe Pos
nextObstacle grid ((x, y), dir) =
  let
    is_left  (x', y') = x' <  x && y' == y
    is_right (x', y') = x' >  x && y' == y
    is_above (x', y') = x' == x && y' <  y
    is_below (x', y') = x' == x && y' >  y
  in
    case dir of
      Lft -> Set.lookupMax $ Set.filter is_left  grid.obstacles
      Rgt -> Set.lookupMin $ Set.filter is_right grid.obstacles
      Up  -> Set.lookupMax $ Set.filter is_above grid.obstacles
      Dwn -> Set.lookupMin $ Set.filter is_below grid.obstacles

type Path = [Config]

pathFrom :: Grid -> Config -> Path
pathFrom grid start_config =
  let
    exit_pos ((_, y), Lft) = (-1, y)
    exit_pos ((_, y), Rgt) = (grid.max_x + 1, y)
    exit_pos ((x, _), Up)  = (x, -1)
    exit_pos ((x, _), Dwn) = (x, grid.max_y + 1)

    move :: Config -> Path
    move config@(_, dir) =
      config : case nextObstacle grid config of
        Nothing -> [(exit_pos config, dir)]
        Just obstacle_pos -> move (next_pos, next_dir)
          where
            next_pos = stepBack dir obstacle_pos
            next_dir = turnRight dir
  in
    move start_config

isLoop :: Path -> Bool
isLoop = 
  let
    go :: Set Config -> Path -> Bool
    go seen_configs [] = False
    go seen_configs (config : rest_path) = 
      config `elem` seen_configs || go (Set.insert config seen_configs) rest_path
  in
    go Set.empty

-- | Expand path to all intermediate configs, not just configs with direction changes.
expand :: Path -> Path
expand path = do
  ((start_pos, dir), (end_pos, _)) <- zip path (tail path)
  inter_pos <- takeWhile (/= end_pos) $ iterate (step dir) start_pos
  return (inter_pos, dir)

main :: IO ()
main = do
  (start_config, grid) <- parse <$> readFile "input/06.txt"

  putStr "Part 1: "
  let path_poses = nubOrd $ map fst $ expand $ pathFrom grid start_config
  print $ length path_poses

  putStr "Part 2: "
  let obstacles_candidates = delete (fst start_config) path_poses
  print $ length $ do
    pos <- obstacles_candidates
    let new_grid = grid { obstacles = Set.insert pos grid.obstacles }
    guard $ isLoop $ pathFrom new_grid start_config
