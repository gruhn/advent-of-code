module Main (main) where
import Data.Map (Map)
import Utils (Parser, parseFile)
import Text.Megaparsec (choice, sepEndBy, endBy, some)
import Text.Megaparsec.Char (char, newline)
import qualified Data.Map as Map
import Control.Monad (guard)
import Data.Foldable (foldl')

type Pos = (Int,Int)

data Dir = Up | Dwn | Lft | Rgt
  deriving Show

step :: Dir -> Pos -> Pos
step dir (x,y) = 
  case dir of
    Up  -> (x,y-1)
    Dwn -> (x,y+1)
    Lft -> (x-1,y)
    Rgt -> (x+1,y)

data Item = Robot | Wall | SmallBox | LargeBox Pos
  deriving (Show, Eq)

type Grid = Map Pos Item

parser :: Parser (Grid, [Dir])
parser = (,) <$> grid <* newline <*> moves
  where
    grid :: Parser (Map Pos Item)
    grid = to_grid <$> some cell `endBy` newline

    cell :: Parser (Maybe Item)
    cell = choice
      [ Just Robot    <$ char '@'
      , Just Wall     <$ char '#'
      , Just SmallBox <$ char 'O'
      , Nothing       <$ char '.'
      ]

    dir :: Parser Dir
    dir = choice
      [ Up  <$ char '^'
      , Dwn <$ char 'v'
      , Lft <$ char '<'
      , Rgt <$ char '>'
      ]

    moves :: Parser [Dir]
    moves = concat <$> some dir `sepEndBy` newline

    to_grid :: [[Maybe Item]] -> Map Pos Item
    to_grid rows = Map.fromList $ do
      (y, row)       <- zip [0..] rows
      (x, Just item) <- zip [0..] row
      return ((x,y), item)

moveItem :: Dir -> Pos -> Grid -> Maybe Grid
moveItem dir pos grid_0 = 
  case Map.lookup pos grid_0 of
    Nothing   -> Just grid_0
    Just Wall -> Nothing
    Just (LargeBox pos_adjacent) -> do
      let new_pos = step dir pos
      let new_pos_adjacent = step dir pos_adjacent
      let grid_1 = Map.delete pos $ Map.delete pos_adjacent grid_0
      grid_2 <- moveItem dir new_pos grid_1
      grid_3 <- moveItem dir new_pos_adjacent grid_2
      return 
        $ Map.insert new_pos (LargeBox new_pos_adjacent)
        $ Map.insert new_pos_adjacent (LargeBox new_pos) grid_3
    Just robot_or_small_box -> do
      let new_pos = step dir pos
      let grid_1 = Map.delete pos grid_0
      grid_2 <- moveItem dir new_pos grid_1
      return $ Map.insert new_pos robot_or_small_box grid_2

moveRobot :: (Pos, Grid) -> Dir -> (Pos, Grid)
moveRobot (robot_pos, grid) dir = 
  case moveItem dir robot_pos grid of
    Nothing       -> (robot_pos, grid)
    Just new_grid -> (step dir robot_pos, new_grid)

getRobotPos :: Grid -> Pos
getRobotPos grid = head [ pos | (pos, Robot) <- Map.toList grid ]

moveRobotAll :: [Dir] -> Grid -> Grid
moveRobotAll moves grid = snd $ foldl' moveRobot (getRobotPos grid, grid) moves

scaleUp :: Grid -> Grid
scaleUp grid = Map.fromList $ do
  ((x,y), item) <- Map.toList grid
  let left_pos  = (x*2  , y)
  let right_pos = (x*2+1, y)
  case item of
    Robot      -> [ (left_pos, Robot) ]
    SmallBox   -> [ (left_pos, LargeBox right_pos), (right_pos, LargeBox left_pos) ]
    Wall       -> [ (left_pos, Wall), (right_pos, Wall) ]
    LargeBox _ -> undefined

main :: IO ()
main = do
  (grid, moves) <- parseFile parser "input/15.txt"

  putStr "Part 1: "
  print $ sum $ do
    ((x,y), SmallBox) <- Map.toList $ moveRobotAll moves grid 
    return $ x + 100 * y

  putStr "Part 2: "
  print $ sum $ do
    ((x,y), LargeBox pos_adjacent) <- Map.toList $ moveRobotAll moves (scaleUp grid)
    guard $ x < fst pos_adjacent
    return $ x + 100 * y
