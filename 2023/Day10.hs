module Main where
import Utils (withCoords, iterateJust)
import Control.Monad (guard)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Foldable (find)
import Data.Maybe (fromJust)
import Data.Set (Set)

type Pos = (Int,Int)

data Dir = North | South | West | East
  deriving Show

step :: Dir -> Pos -> Pos
step dir (x,y) =
  case dir of
    North -> (x,y-1)
    East  -> (x+1,y)
    South -> (x,y+1)
    West  -> (x-1,y)

turn :: Dir -> Char -> Maybe Dir
turn dir tile =
  case (dir, tile) of
    (North, '|') -> Just North
    (South, '|') -> Just South
    (West , '-') -> Just West
    (East , '-') -> Just East
    (South, 'L') -> Just East
    (West , 'L') -> Just North
    (East , 'J') -> Just North
    (South, 'J') -> Just West
    (North, 'F') -> Just East
    (West , 'F') -> Just South
    (East , '7') -> Just South
    (North, '7') -> Just West
    _ -> Nothing

next :: Map Pos Char -> (Pos, Dir) -> Maybe (Pos, Dir)
next tiles (pos, dir) = do
  let new_pos = step dir pos
  tile    <- Map.lookup new_pos tiles
  new_dir <- turn dir tile
  return (new_pos, new_dir)

neighbors :: Pos -> Set Pos
neighbors (x,y) = Set.fromList $ do
  dx <- [-1 .. 1]
  dy <- [-1 .. 1]
  guard $ (dx,dy) /= (0,0)
  return (x+dx,y+dy)

connected :: Set Pos -> Set Pos -> Set Pos -> Set Pos
connected support_set boundary poses = go poses poses
  where
    expand :: Pos -> Set Pos -> Set Pos
    expand pos accum =
        Set.intersection support_set
      $ (Set.\\ boundary)
      $ (Set.\\ accum)
      $ neighbors pos Set.\\ boundary

    go :: Set Pos -> Set Pos -> Set Pos
    go accum worklist =
      case Set.minView worklist of
        Nothing -> accum
        Just (pos, rest_worklist) ->
          let new_poses = expand pos accum in
          go (new_poses <> accum) (new_poses <> rest_worklist)

surface :: Set Pos -> Set Pos
surface pos_set = Set.filter ((<8) . neighbor_count) pos_set
  where
    neighbor_count :: Pos -> Int
    neighbor_count pos =
      length $ Set.intersection pos_set (neighbors pos)

partitionExterior :: Set Pos -> Set Pos -> (Set Pos, Set Pos)
partitionExterior pos_set boundary =
  let exterior = connected pos_set boundary $ surface pos_set Set.\\ boundary
   in (exterior, pos_set Set.\\ exterior)

scaleUp :: Pos -> Pos
scaleUp (x,y) = (x*2, y*2)

scaleDown :: Pos -> Pos
scaleDown (x,y) = (x `div` 2, y `div` 2)

collectEnclosed :: Set Pos -> [Pos] -> Set Pos
collectEnclosed pos_set path = original_enclosed
  where
    -- scale up all positions so enclosed positions are connected:
    path_scaled = map scaleUp path
    path_filled = Set.fromList $ fill path_scaled

    pos_set_scaled = Set.map scaleUp pos_set 
    pos_set_filled = pos_set_scaled <> Set.unions (Set.map neighbors pos_set_scaled)

    exterior :: Set Pos
    exterior = 
        connected pos_set_filled path_filled 
      $ surface pos_set_filled Set.\\ path_filled

    interior :: Set Pos
    interior = pos_set_filled Set.\\ exterior Set.\\ path_filled

    is_original_pos :: Pos -> Bool
    is_original_pos (x,y) = even x && even y

    original_enclosed :: Set Pos
    original_enclosed = Set.map scaleDown $ Set.filter is_original_pos interior

    filler :: Pos -> Pos -> Pos
    filler (x1,y1) (x2,y2)
      | x1 == x2 = (x1, min y1 y2 + 1)
      | y1 == y2 = (min x1 x2 + 1, y1)
      | otherwise = undefined

    fill :: [Pos] -> [Pos]
    fill [] = []
    fill [pos] = [pos, filler pos (head path_scaled)]
    fill (pos1:pos2:rest_path) =
      pos1 : filler pos1 pos2 : fill (pos2:rest_path)

main :: IO ()
main = do
  input <- withCoords . lines <$> readFile "input/10.txt"

  let start :: Pos
      start = fst $ fromJust $ find ((=='S') . snd) input

      tiles :: Map Pos Char
      tiles = Map.fromList input

      loop_path :: [Pos]
      loop_path = head $ do
        dir <- [North, East, South, West]
        let path = iterateJust (next tiles) (start, dir)
        guard $ length path > 1
        return $ map fst path

  putStr "Part 1: "
  print $ length loop_path `div` 2

  putStr "Part 2: "
  print $ length $ collectEnclosed (Map.keysSet tiles) loop_path
