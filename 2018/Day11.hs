module Main (main) where
import Data.Function ((&))
import Data.Foldable (maximumBy)
import Control.Monad (forM_)
import Data.Ord (comparing)
import Data.Map
import qualified Data.Map as Map
import Control.Monad.Trans.State (State)
import qualified Control.Monad.Trans.State as State
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Maybe (fromMaybe)

type Cell = (Int,Int)

type Square = (Int,Int,Int)

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

-- | See: https://en.wikipedia.org/wiki/Summed-area_table
type SummedAreaTable = Vector (Vector Int)

lookupCell :: Cell -> SummedAreaTable -> Int
lookupCell (x,y) table = fromMaybe 0 $ do
  row <- table Vector.!? y
  row Vector.!? x

tableFor :: Int -> SummedAreaTable
tableFor grid_serial_number = 
  let
    go :: Cell -> State (Map Cell Int) Int
    go (x,y) =
      if x > 300 || y > 300 then 
        return 0
      else do
        table <- State.get
        case Map.lookup (x,y) table of
          Just value -> return value
          Nothing    -> do
            value_south_east <- go (x+1,y+1)
            value_east       <- go (x+1,y)
            value_south      <- go (x,y+1)
            let value_cell = cellPowerLevel grid_serial_number (x,y)
                value_total = value_cell + value_east + value_south - value_south_east
            State.modify (Map.insert (x,y) value_total)
            return value_total

    -- After construction, we do a lot of lookups in `SummedAreaTable` but no writes.
    -- So converting the `Map Cell Int` to a `Vector (Vector Int)` seems to give almost
    -- 2x speed up.
    to_vector :: Map Cell Int -> Vector (Vector Int)
    to_vector dict = 
      Vector.generate 300 (\y -> Vector.generate 300 (\x -> dict Map.! (x,y)))
  in
    to_vector $ State.execState (go (1,1)) Map.empty

squarePowerLevel :: SummedAreaTable -> Square -> Int
squarePowerLevel table (x,y,len) = 
  let 
    top_left  = lookupCell (x,y) table
    top_right = lookupCell (x+len,y) table
    bot_left  = lookupCell (x,y+len) table
    bot_right = lookupCell (x+len,y+len) table
  in
    top_left - top_right - bot_left + bot_right
      
bestSquarePerSize :: (Square -> Int) -> [Square]
bestSquarePerSize score = do
  len <- [1..300]
  return $ maximumBy (comparing score) $ do 
    x <- [1..300-len+1]
    y <- [1..300-len+1]
    return (x,y,len)

main :: IO ()
main = do
  -- putStr "TESTS PASS: "
  -- print
  --   [ cellPowerLevel 8 (3,5) == 4
  --   , cellPowerLevel 57 (122,79) == -5
  --   , cellPowerLevel 39 (217,196) == 0
  --   , cellPowerLevel 71 (101,153) == 4
  --   , squarePowerLevel (tableFor 18) (33,45,3) == 29
  --   , squarePowerLevel (tableFor 42) (21,61,3) == 30
  --   , squarePowerLevel (tableFor 18) (90,269,16) == 113
  --   , squarePowerLevel (tableFor 42) (232,251,12) == 119
  --   ]
  -- putStrLn ""

  let score = squarePowerLevel (tableFor 4151)
      best_squares = bestSquarePerSize score

  forM_ best_squares $ \(x,y,len) -> do
    let len_str   = show len ++ "x" ++ show len
    let score_str = show (score (x,y,len))
    putStrLn $ "best " ++ len_str ++ " square at " ++ show (x,y) ++ " / score: " ++ score_str

  putStr "Part 1: "
  print $ best_squares !! (3-1)

  putStr "Part 2: "
  print $ maximumBy (comparing score) best_squares
