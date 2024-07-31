module Main (main) where
import Data.Function ((&))
import Data.Foldable (maximumBy, traverse_)
import Control.Monad (guard, when)
import Control.Monad.Trans.State (State, StateT)
import Data.Map (Map)
import qualified Control.Monad.Trans.State as State
import qualified Data.Map as Map
import Data.Foldable1 (foldrM1)
import Data.List.NonEmpty (NonEmpty((:|)), fromList)
import Debug.Trace
import Data.Ord (comparing)
import Data.List (sort, sortOn)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as StateT
import Data.Maybe (catMaybes)

type Cell = (Int, Int)

data Square = Square
  { topLeftCell :: Cell
  , sideLength  :: Int
  } deriving (Show, Eq, Ord)

cellPowerLevel :: Int -> Cell -> Int
cellPowerLevel grid_serial_number (x,y) =
  let
    -- Find the fuel cell's rack ID, which is its X coordinate plus 10.
    rack_id = x + 10
  in
    -- Begin with a power level of the rack ID times the Y coordinate.
    rack_id * y
      -- Increase the power level by the value of the grid serial number (your puzzle input).
      & (+ grid_serial_number)
      -- Set the power level to itself multiplied by the rack ID.
      & (* rack_id)
      -- Keep only uhe hundreds digit of the power level (so 12345 becomes 3; numbers with no hundreds digit become 0).
      & (`mod` 1000) & (`div` 100)
      -- Subtract 5 from the power level.
      & subtract 5

squarePowerLevel :: Int -> Square -> Int
squarePowerLevel grid_serial_number (Square (x,y) len) =
  let
    square = [ (x+dx,y+dy) | dx <- [0..len-1], dy <- [0..len-1] ]
  in
    sum $ map (cellPowerLevel grid_serial_number) square

isInBounds :: Square -> Bool
isInBounds (Square (x,y) len) =
  1 <= x && x+len-1 <= 300 && 1 <= y && y+len-1 <= 300

upperBound :: (Cell -> Int) -> Square -> Int
upperBound score (Square (x,y) len) = sum $ do
  x' <- [x..x+len-1]
  y' <- [y..y+len-1]
  let s = score (x',y')
  guard $ s > 0
  return s

shrink :: Square -> [Square]
shrink (Square (x,y) len) = do
  guard $ len > 1
  top_left <- [ (x,y), (x+1,y), (x,y+1), (x+1,y+1) ]
  return $ Square top_left (len-1)

shift :: Square -> [Square]
shift (Square (x,y) len) = do
  dx <- [ -1, 0, 1 ]
  dy <- [ -1, 0, 1 ]
  guard $ (dx,dy) /= (0, 0)
  let square = Square (x+dx, y+dy) len
  guard $ isInBounds square
  return square

search2 :: Int -> Square -> State Square Square
search2 grid_serial_number square = do
  best_square <- State.get
  let best_score_yet = squarePowerLevel grid_serial_number best_square
  traceShowM (best_square, best_score_yet)
  let score = squarePowerLevel grid_serial_number square
  State.put (if score > best_score_yet then square else best_square)
  case filter (\sq -> upperBound (cellPowerLevel grid_serial_number) sq > best_score_yet) (shrink square ++ shift square) of
    []     -> return square
    sq:sqs -> do 
      sqs' <- traverse (search2 grid_serial_number) (sq:sqs)
      return $ maximumBy (comparing (squarePowerLevel grid_serial_number)) (square : sqs')

main :: IO ()
main = do
  putStr "TESTS PASS: "
  print
    [ cellPowerLevel 8 (3,5) == 4
    , cellPowerLevel 57 (122,79) == -5
    , cellPowerLevel 39 (217,196) == 0
    , cellPowerLevel 71 (101,153) == 4
    , squarePowerLevel 18 (Square (33,45) 3) == 29
    , squarePowerLevel 42 (Square (21,61) 3) == 30
    , squarePowerLevel 18 (Square (90,269) 16) == 113
    , squarePowerLevel 42 (Square (232,251) 12) == 119
    ]

  let grid_serial_number = 4151

  putStr "Part 1: "
  -- let all_3x3_squares = [ Square (x,y) 3 | x <- [1..300-2], y <- [1..300-2] ]
  -- print $ topLeftCell $ maximumBy (compare `on` squarePowerLevel grid_serial_number) all_3x3_squares

  putStr "Part 2: "
  -- traverse_ print $ scanl max minBound (map (squarePowerLevel 16 . traceShowId) $ allSquares 1)
  let score = squarePowerLevel 18
      initial_square = Square (1,1) 300
  print $ StateT.evalStateT (search2 18 initial_square) initial_square
  -- traverse_ print 
  --   $ map (\sq -> (sq, score sq))
  --   $ greedySearch score (Square (1,1) 300)
  -- print $ maximumBy (compare `on` score) $ map (growGreedy score) all_1x1_squares

  -- putStrLn "=========================="
  -- print $ length $ takeWhile (>0) $ sortOn negate $ do
  --   x <- [1 .. 300]
  --   y <- [1 .. 300]
  --   return $ cellPowerLevel 18 (x,y)

  -- print $ takeWhile (<0) $ sort $ do
  --   x <- [1 .. 300]
  --   y <- [1 .. 300]
  --   return $ cellPowerLevel 18 (x,y)

chunks :: Int -> [a] -> [[a]]
chunks n [] = []
chunks n as =
  let
    (chunk, rest) = splitAt n as
  in
    chunk : chunks n rest
