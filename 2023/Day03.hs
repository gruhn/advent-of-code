module Main where
import Utils (withCoords)
import Data.List (groupBy)
import Data.Function (on)
import Data.Char (isDigit)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad (guard)
import Data.Maybe (mapMaybe)
import Data.Containers.ListUtils (nubOrd, nubOrdOn)

type Point = (Int,Int)

neighbors :: Point -> [Point]
neighbors (x,y) = do 
  dx <- [-1,0,1]
  dy <- [-1,0,1]
  guard $ (dx,dy) /= (0,0)
  return (x+dx,y+dy)

numbersAdjacentTo :: Map Point (Int, [Point]) -> Point -> [Int]
numbersAdjacentTo number_dict pos = 
  -- only keep number itself
    map fst
  -- identify duplicates by comparing full list points
  $ nubOrdOn snd 
  -- lookup neighboring numbers to `pos`
  $ mapMaybe (`Map.lookup` number_dict) 
  $ neighbors pos

main :: IO ()
main = do
  input <- withCoords . lines <$> readFile "input/03.txt"

  let numbers :: [(Int, [Point])]
      numbers = do
        -- group verticlally adjacent digits and non-digits:
        group <- groupBy ((&&) `on` isDigit . snd) input
        let (points, digits) = unzip group
        -- filter out non-digit groups:
        guard $ all isDigit digits
        -- parse number from digits:
        return (read digits :: Int, points)

      -- lookup table for numbers
      number_dict :: Map Point (Int, [Point])
      number_dict = Map.fromList $ do
        (number, points) <- numbers
        point <- points
        return (point, (number, points))

      -- lookup table for symbols other than '.'
      symbols :: Map Point Char
      symbols = Map.fromList $ do 
        (point, char) <- input
        guard $ not $ isDigit char
        guard $ char /= '.'
        return (point, char)

  putStr "Part 1: "
  print $ sum $ do
    (number, digit_positions) <- numbers
    -- collect neighbor positions of all digits:
    let all_neighbors = nubOrd $ concatMap neighbors digit_positions
    -- keep number if any position contains a symbol:
    guard $ any (`Map.member` symbols) all_neighbors
    return number

  putStr "Part 2: "
  print $ sum $ do
    -- filter out gears from symbols and get adjacent numbers:
    (gear_position, gear) <- Map.toList symbols
    guard $ gear == '*'
    let adjacent_numbers = numbersAdjacentTo number_dict gear_position
    -- make sure star has exactly two neighboring numbers:
    guard $ length adjacent_numbers == 2
    -- multiply resulting numbers:
    return $ product adjacent_numbers
