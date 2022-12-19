module Main where
import Utils (Parser, parseHardError)
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (newline, string, hspace)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import qualified Data.Set as S
import Control.Monad (guard)

type Point = (Int,Int)

type Sensor = (Point, Point)

parser :: Parser [Sensor] 
parser = sensor `sepBy` newline
  where
    integer :: Parser Int
    integer = signed hspace decimal

    sensor :: Parser Sensor
    sensor = (,)
      <$ string "Sensor at "            <*> point <* string ": "
      <* string "closest beacon is at " <*> point

    point :: Parser Point
    point = (,)
      <$ string "x=" <*> integer <* string ", "
      <* string "y=" <*> integer

manhattanDist :: Point -> Point -> Int
manhattanDist (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

p1 :: Int -> S.Set (Point, Point) -> S.Set Point
p1 row = S.unions . S.map (uncurry go) 
  where
    go :: Point -> Point -> S.Set Point
    go sensor@(x,y) beacon = S.delete beacon $ S.fromList (left <> right)
      where
        max_dist = manhattanDist sensor beacon

        left = takeWhile ((<= max_dist) . manhattanDist sensor) [ (x+d,row) | d <- [0..] ]
        right = takeWhile ((<= max_dist) . manhattanDist sensor) [ (x-d,row) | d <- [1..] ]

border :: (Point, Point) -> [Point]
border (sensor, beacon) = do 
  let dist = manhattanDist sensor beacon + 1
      (x,y) = sensor

  dx <- [0 .. dist]

  let (x',y') = (x+dx, y+dist-dx)

  guard (0 <= x' && x' <= 4000000)
  guard (0 <= y' && y' <= 4000000)

  return (x',y')

inReach :: [(Point, Point)] -> Point -> Bool
inReach  sensors point = any go sensors
  where
    go :: (Point,Point) -> Bool
    go (sensor, beacon) = 
      manhattanDist sensor beacon >= manhattanDist sensor point

tuningFrequency :: (Int, Int) -> Int
tuningFrequency (x,y) = x*4000000 + y

main :: IO ()
main = do
  input <- parseHardError parser <$> readFile "input/15.txt"

  putStr "Part 1: "
  print $ S.size $ p1 2000000 $ S.fromList input

  putStr "Part 2: "

  print 
    $ filter (not . inReach input)
    $ concat
    $ border <$> input
  -- print $ head $ uncurry border <$> input