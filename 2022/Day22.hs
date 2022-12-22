module Main where
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Set as S
import Control.Monad (guard)
import qualified Data.Map as M
import Data.List (groupBy, transpose, scanl')
import Utils (chunksOf, parseHardError, Parser, withCoords)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (char)
import Control.Applicative ( some, (<|>) )
import Text.Megaparsec (choice)
import Data.Foldable ( foldl', traverse_ )
import Debug.Trace (traceShowId, traceShow)
import Data.Function (on)

type Point = (Int,Int)

data Face = Face
  { getSize :: Int
  , getWalls :: Set Point
  , getPos :: Point
  } deriving Show

type Block = [String]

data Dir = Up | Dwn | Lft | Rgt
  deriving (Eq, Ord)

instance Show Dir where
  show Up = "^"
  show Dwn = "v"
  show Lft = "<"
  show Rgt = ">"

data Move = TurnRight | TurnLeft | Step
  deriving Show

parseFaces :: Int -> [String] -> Map Point Face
parseFaces size rows = faces
  where
    blockwise_rows :: [[Block]]
    blockwise_rows = transpose <$> chunksOf size (chunksOf size <$> rows)

    walls_from :: Block -> Set Point
    walls_from = M.keysSet . M.filter (=='#') . M.fromList . withCoords

    face_grid :: [[Set Point]]
    face_grid = fmap walls_from <$> blockwise_rows

    faces :: Map Point Face
    faces = M.fromList $ do
      (pos, walls) <-  withCoords face_grid
      guard (not . null $ walls)
      return (pos, Face size walls pos)

parseMoves :: String -> [Move]
parseMoves = concat <$> parseHardError moves
  where
    moves = some (turn_right <|> turn_left <|> step)

    turn_right = [TurnRight] <$ char 'R'
    turn_left  = [TurnLeft]  <$ char 'L'

    step :: Parser [Move]
    step = (`replicate` Step) <$> decimal

type State = (Dir, Point, Face)

turnRight :: Dir -> Dir
turnRight dir = dropWhile (/=dir) (cycle [Up,Rgt,Dwn,Lft]) !! 1

turnLeft :: Dir -> Dir
turnLeft dir = dropWhile (/=dir) (cycle [Up,Lft,Dwn,Rgt]) !! 1

move :: (Dir -> Face -> Point -> (Dir, Face, Point)) -> State -> Move -> State
move next_face (dir, pos, face) TurnRight = (turnRight dir, pos, face)
move next_face (dir, pos, face) TurnLeft  = (turnLeft dir, pos, face)
move next_face (dir, pos, face) Step
  | is_wall new_pos new_face = (dir, pos, face)
  | otherwise = (new_dir, new_pos, new_face)
  where
    Face size walls _ = face

    (new_dir, new_face, new_pos) =
      if out_of_bounds (step dir pos) then
        next_face dir face (step dir pos)
      else
        (dir, face, step dir pos)

    out_of_bounds :: Point -> Bool
    out_of_bounds (x,y) =
      x < 0 || y < 0 || size <= x || size <= y

    is_wall :: Point -> Face -> Bool
    is_wall point face =
      S.member point (getWalls face)

    step :: Dir -> Point -> Point
    step Up  (x,y) = (x, y-1)
    step Dwn (x,y) = (x, y+1)
    step Lft (x,y) = (x-1, y)
    step Rgt (x,y) = (x+1, y)

main :: IO ()
main = do
  input <- lines <$> readFile "input/22.txt"

  let moves = parseMoves $ last input

      -- faces = parseFaces 4 $ take (length input - 2) input
      -- start_state = (Rgt, (0,0), faces M.! (2,0))

      -- face_graph = M.fromList
      --   [ (((2,0), Up), ((0,1), Up)), (((2,0), Dwn), ((2,1), Up)), (((2,0), Lft), ((3,2), Up)), (((2,0), Rgt), ((1,1), Rgt))
      --   , (((2,1), Up), ((2,0), Dwn)), (((2,1), Dwn), ((2,2), Up)), (((2,1), Lft), ((1,1), Rgt)), (((2,1), Rgt), ((3,2), Up))
      --   , (((1,1), Up), ((2,0), Lft)), (((1,1), Dwn), ((2,2), Lft)), (((1,1), Lft), ((0,1), Rgt)), (((1,1), Rgt), ((2,1), Lft))
      --   , (((0,1), Up), ((2,0), Up)), (((0,1), Dwn), ((2,2), Dwn)), (((0,1), Lft), ((3,2), Dwn)), (((0,1), Rgt), ((1,1), Lft))
      --   , (((2,2), Up), ((2,1), Dwn)), (((2,2), Dwn), ((0,1), Dwn)), (((2,2), Lft), ((1,1), Dwn)), (((2,2), Rgt), ((3,2), Lft))
      --   , (((3,2), Up), ((2,1), Rgt)), (((3,2), Dwn), ((0,1), Lft)), (((3,2), Lft), ((2,2), Rgt)), (((3,2), Rgt), ((3,2), Rgt))
      --   ]


      faces = parseFaces 50 $ take (length input - 2) input
      start_state = (Rgt, (0,0), faces M.! (1,0))

      face_graph = M.fromList
        [ (((1,0), Up), ((0,3), Lft)), (((1,0), Dwn), ((1,1), Up)), (((1,0), Lft), ((0,2), Lft)), (((1,0), Rgt), ((2,0), Lft))
        , (((2,0), Up), ((0,3), Dwn)), (((2,0), Dwn), ((1,1), Rgt)), (((2,0), Lft), ((1,0), Rgt)), (((2,0), Rgt), ((1,2), Rgt))
        , (((1,1), Up), ((1,0), Dwn)), (((1,1), Dwn), ((1,2), Up)), (((1,1), Lft), ((0,2), Up)), (((1,1), Rgt), ((2,0), Up))
        , (((1,2), Up), ((1,1), Dwn)), (((1,2), Dwn), ((0,3), Rgt)), (((1,2), Lft), ((0,2), Rgt)), (((1,2), Rgt), ((2,0), Rgt))
        , (((0,2), Up), ((1,1), Lft)), (((0,2), Dwn), ((0,3), Up)), (((0,2), Lft), ((1,0), Lft)), (((0,2), Rgt), ((1,2), Lft))
        , (((0,3), Up), ((0,2), Dwn)), (((0,3), Dwn), ((2,0), Up)), (((0,3), Lft), ((1,0), Up)), (((0,3), Rgt), ((1,2), Dwn))
        ]

      adjust_orientation face_size dir1 dir2 = (!! iters) . iterate (shift . rotate90) . wrap_around
        where
          shift (x,y) = (x+face_size-1,y)

          wrap_around :: Point -> Point
          wrap_around (x,y) = (x `mod` face_size, y `mod` face_size)

          rotate90 :: Point -> Point
          rotate90 (x,y) = (-y, x)

          iters =
            case (dir1, dir2) of
              (Lft, Lft) -> 2; (Lft, Rgt) -> 0; (Lft, Up) -> 3; (Lft, Dwn) -> 1
              (Rgt, Lft) -> 0; (Rgt, Rgt) -> 2; (Rgt, Up) -> 1; (Rgt, Dwn) -> 3
              (Up, Lft) -> 1; (Up, Rgt) -> 3; (Up, Up) -> 2; (Up, Dwn) -> 0
              (Dwn, Lft) -> 3; (Dwn, Rgt) -> 1; (Dwn, Up) -> 0; (Dwn, Dwn) -> 2

      next_face :: Dir -> Face -> Point -> (Dir, Face, Point)
      next_face dir face pos = (new_dir, new_face, new_pos)
        where
          (new_face_pos, orientation) = face_graph M.! (getPos face, dir)
          new_face = faces M.! new_face_pos
          new_pos = adjust_orientation (getSize new_face) dir orientation pos

          flip_dir Lft = Rgt
          flip_dir Rgt = Lft
          flip_dir Up = Dwn
          flip_dir Dwn = Up

          new_dir = flip_dir orientation

  putStr "Part 2: "

  let state_history = scanl' (move next_face) start_state moves
      final_state = last state_history

      absolute_pos (dir, (x,y), Face size _ (fx,fy)) = (fx*size + x, fy*size + y)

      dir_value dir = case dir of Rgt -> 0; Dwn -> 1; Lft -> 2; Up -> 3

      same_dir_pos (dir1, pos1, _) (dir2, pos2, _) = (dir1, pos1) == (dir2, pos2)

      state_history_compressed = head <$> groupBy same_dir_pos state_history

  -- print $ getWalls $ faces M.! (2,0)
  -- print moves
  -- traverse_ print $ (\s@(_,pos,f) -> (absolute_pos s, pos, getPos f)) <$> state_history
  -- print $ absolute_pos <$> state_history
  -- print $ move next_face start_state Step

  -- print "\n"
  -- traverse_ (putStrLn . showGrid (M.elems faces)) $ 
  --   take 10 (chunksOf 100 state_history_compressed)

  print $
    let final_state = last state_history
        (final_dir, _, _) = final_state
        (final_column, final_row) = absolute_pos final_state
    in dir_value final_dir + 1000*(final_row+1) + 4*(final_column+1)

  print $
    let final_state = last state_history
        (final_dir, _, _) = final_state
        (final_column, final_row) = absolute_pos final_state
    in (final_column+1, final_row+1, dir_value final_dir)

-- 183068 too high
--  16319 too low

absolutePath :: [(Dir, Point, Face)] -> [(Point, Dir)]
absolutePath = fmap go
  where
    go :: (Dir, Point, Face) -> (Point, Dir)
    go (dir, (x,y), Face size _ (fx,fy)) = ((fx*size + x, fy*size + y), dir)

showGrid :: [Face] -> [(Dir, Point, Face)] -> String
showGrid faces state_history = unlines rows
  where
    absolute_pos (Face size _ (fx,fy)) (x,y) = (fx*size + x, fy*size + y)

    walls_absolute face = S.map (absolute_pos face) (getWalls face)

    all_walls_absolute = S.unions (walls_absolute <$> faces)
    path_map = M.fromList $ do
      (dir, pos, face) <- state_history
      return (absolute_pos face pos, dir)

    max_x = S.findMax (S.map fst all_walls_absolute)
    max_y = S.findMax (S.map snd all_walls_absolute)

    row y = do
      x <- [0 .. max_x]
      case (M.lookup (x,y) path_map, S.member (x,y) all_walls_absolute) of
        (Just dir, __) -> show dir
        (Nothing, False) -> "."
        (Nothing, True) -> "#"

    rows = row <$> [0 .. max_y]

manhattanDist :: Point -> Point -> Int
manhattanDist (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)
