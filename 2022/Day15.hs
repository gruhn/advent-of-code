module Main where
import Utils (Parser, parseHardError)
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (newline, string, hspace)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Data.Function (on)
import Data.Foldable (maximumBy, find, minimumBy)
import qualified Data.Set as S
import Control.Monad (guard)

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

getPoints :: Rect -> [Point] 
getPoints (Rect left top right bottom) = 
  [ (fromIntegral x, fromIntegral y) | x <- [left..right], y <- [top..bottom] ]

data Rect = Rect 
  { getLeft   :: Integer
  , getTop    :: Integer 
  , getRight  :: Integer
  , getBottom :: Integer }
  deriving Show

toRect :: (Point, Point) -> Rect
toRect (sensor, beacon) = Rect left top right bottom
  where
    reach  = manhattanDist sensor beacon
    (x, y) = rotateForth sensor
    left   = fromIntegral $ x - reach
    top    = fromIntegral $ y - reach
    right  = fromIntegral $ x + reach
    bottom = fromIntegral $ y + reach

rotateForth :: Point -> Point
rotateForth (x,y) = (x-y, x+y)

rotateBack :: Point -> Point
rotateBack (x,y) = ((x+y) `div` 2, (-x+y) `div` 2)

area :: Rect -> Integer
area (Rect left top right bottom) = abs (width * height) * sign
  where
    width = right - left + 1
    height = bottom - top + 1
    sign = min (signum width) (signum height)

data Axis = X Integer | Y Integer

slice :: Axis -> Rect -> (Rect, Rect)
slice (X x) (Rect left top right bottom) = 
  ( Rect left top (min right x) bottom
  , Rect (max left (x+1)) top right bottom )
slice (Y y) (Rect left top right bottom) = 
  ( Rect left top right (min bottom y)
  , Rect left (max top (y+1)) right bottom )

intersection :: Rect -> Rect -> Rect
intersection (Rect left1 top1 right1 bottom1) (Rect left2 top2 right2 bottom2) =
  Rect 
    (max left1 left2)
    (max top1 top2)
    (min right1 right2)
    (min bottom1 bottom2)

overlapArea :: Rect -> Rect -> Integer
overlapArea rect1 rect2 = 
  max 0 $ area $ intersection rect1 rect2

difference :: Rect -> Rect -> [Rect]
difference rect0 (Rect left top right bottom) = parts_with_positive_area
  where
    (left_part, rect1)  = slice (X $ left-1) rect0
    (top_part, rect2)   = slice (Y $ top-1)  rect1
    (rect3, right_part) = slice (X right)  rect2
    (_, bottom_part)    = slice (Y bottom) rect3

    parts_with_positive_area = filter ((> 0) . area) 
      [ left_part, top_part, right_part, bottom_part ]

differences :: Rect -> [Rect] -> [Rect]
differences base_rect [] = [base_rect]
differences base_rect rects = do
  -- Find the rect with most overlap and subtract it from base_rect.
  let rect_most_overlap = maximumBy (compare `on` overlapArea base_rect) rects
  -- That leaves at most four sub-rectangles of base_rect on which we recursively apply `differences`.
  rem_base_rect <- difference base_rect rect_most_overlap
  -- Only pass rects that actually have overlap with the sub-rectangle in the recursive call.
  let rects_any_overlap = filter ((> 0) . overlapArea rem_base_rect) rects
  differences rem_base_rect rects_any_overlap

boundingBox :: [Rect] -> Rect
boundingBox rects = Rect
  (minimum $ getLeft   <$> rects)
  (minimum $ getTop    <$> rects)
  (maximum $ getRight  <$> rects)
  (maximum $ getBottom <$> rects)

part1 :: Int -> S.Set (Point, Point) -> S.Set Point
part1 row = S.unions . S.map (uncurry go) 
  where
    go :: Point -> Point -> S.Set Point
    go sensor@(x,y) beacon = S.delete beacon $ S.fromList (left <> right)
      where
        max_dist = manhattanDist sensor beacon

        left = takeWhile ((<= max_dist) . manhattanDist sensor) [ (x+d,row) | d <- [0..] ]
        right = takeWhile ((<= max_dist) . manhattanDist sensor) [ (x-d,row) | d <- [1..] ]

main :: IO ()
main = do
  sensors <- parseHardError parser <$> readFile "input/15.txt"

  putStr "Part 1: "
  print $ S.size $ part1 2000000 $ S.fromList sensors

  putStr "Part 2: "
  let rects = toRect <$> sensors
      uncovered = differences (boundingBox rects) rects
  print $ do 
    point <- head . getPoints <$> find ((== 1) . area) uncovered
    let (x,y) = rotateBack point
    -- (2889465,3040754)
    return (x*4000000 + y)
