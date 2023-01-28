module Main where

import Utils (Parser, parseHardError)
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (newline, string, hspace)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Data.Function (on)
import Data.Foldable (maximumBy, find, minimumBy)
import qualified Data.Set as S
import Control.Monad (guard)
import Data.List (sortOn)
import Data.Containers.ListUtils ( nubOrd )
import qualified Cuboid
import Cuboid ( Cuboid(Cuboid), Range(Range) )

type Point = (Int,Int)

parser :: Parser [(Point, Point)] 
parser = sensor `sepBy` newline
  where
    integer :: Parser Int
    integer = signed hspace decimal

    sensor :: Parser (Point, Point)
    sensor = (,)
      <$ string "Sensor at "            <*> point <* string ": "
      <* string "closest beacon is at " <*> point

    point :: Parser Point
    point = (,)
      <$ string "x=" <*> integer <* string ", "
      <* string "y=" <*> integer

manhattanDist :: Point -> Point -> Int
manhattanDist (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

toCuboid :: (Point, Point) -> Cuboid
toCuboid (sensor, beacon) = Cuboid [Range left right, Range top bottom]
  where
    reach  = manhattanDist sensor beacon
    (x, y) = rotateForth sensor
    left   = x - reach
    top    = y - reach
    right  = x + reach
    bottom = y + reach

rotateForth :: Point -> Point
rotateForth (x,y) = (x-y, x+y)

rotateBack :: Point -> Point
rotateBack (x,y) = ((x+y) `div` 2, (-x+y) `div` 2)

coveredRangesAtRow :: Int -> [(Point, Point)] -> [Range]
coveredRangesAtRow row sensors = rows
  where
    get_row :: (Point, Point) -> Range
    get_row (sensor@(x,y), beacon) = 
      let max_dist = manhattanDist sensor beacon
          radius = max_dist - abs (y - row)
      in Range (x-radius) (x+radius)
      
    rows = Cuboid.reduce $ get_row <$> sensors

main :: IO ()
main = do
  sensors <- parseHardError parser <$> readFile "input/15.txt"

  putStr "Part 1: "
  let beacons_at_row = length . nubOrd . filter ((== 2000000) . snd) $ fmap snd sensors
  let covered_points_at_row = sum $ max 0 . Cuboid.diameter <$> coveredRangesAtRow 2000000 sensors
  print $ covered_points_at_row - beacons_at_row  

  putStr "Part 2: "
  print $ do 
    let cuboids = toCuboid <$> sensors
    let uncovered = Cuboid.subtractAll (Cuboid.boundingBox cuboids) cuboids

    rect  <- filter ((== 1) . Cuboid.volume) uncovered
    [x,y] <- Cuboid.points rect

    let (x',y') = rotateBack (x,y)
    -- (2889465,3040754)
    return (x'*4000000 + y')
