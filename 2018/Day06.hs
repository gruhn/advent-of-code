module Main where

import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (string, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import ParseUtils (Parser, parseWith)
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Applicative ((<|>))
import Control.Monad (guard)

select :: [a] -> [(a,[a])]
select []     = []
select (a:as) = (a,as) : [ (a',a:as') | (a',as') <- select as ]

type Pos = (Int, Int)

parser :: Parser [Pos]
parser = pos `sepEndBy` newline
  where
    pos :: Parser Pos
    pos = do      
      x <- decimal
      string ", "
      y <- decimal
      return (x, y)

dist :: Pos -> Pos -> Int
dist (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

data Size = Finite Int | Infinite

neighbors :: Pos -> [Pos]
neighbors (x,y) = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]

regionOf :: Pos -> [Pos] -> Set Pos
regionOf center other_centers = expand Set.empty (Set.singleton center)
  where
    expand :: Set Pos -> Set Pos -> Set Pos
    expand region frontier =
      if Set.null frontier then 
        region
      else 
        expand (frontier <> region) $ Set.fromList $ do
          pos <- Set.toList frontier
          neighbor <- neighbors pos
          guard $ dist center neighbor > dist center pos
          guard $ all (\c -> dist center neighbor < dist c neighbor) other_centers
          return neighbor

data Boundary = North | South | East | West
  deriving (Eq, Ord, Show)

{-| 

        y   x=y
    \   |   /
     \  |  /
      \ | /
       \|/
 -------*------- x
       /|\
      / | \
     /  |  \
    /   |   \
           -x=y
-}
originBoundaries :: Pos -> [Boundary]
originBoundaries (x, y) = 
  let
    north = [ North | -x <= y && x <= y ]
    south = [ South | -x >= y && x >= y ]
    west  = [ West  | -x >= y && x <= y ]
    east  = [ East  | -x <= y && x >= y ]
  in
    north <|> south <|> east <|> west

isBounded :: Pos -> [Pos] -> Bool
isBounded (x1, y1) other_centers = 
  let
    all_boundaries = Set.fromList $ do
      (x2, y2) <- other_centers
      originBoundaries (x2-x1, y2-y1)
  in
    Set.fromList [North,South,East,West] == all_boundaries

regionSize :: Pos -> [Pos] -> Size
regionSize center other_centers = 
  if isBounded center other_centers then
    Finite $ Set.size $ regionOf center other_centers
  else 
    Infinite

type Range = (Int, Int)

rangeWidth :: Range -> Int
rangeWidth (start, end) = end - start + 1

splitRange :: Range -> [Range]
splitRange range@(start, end) = 
  case rangeWidth range of
    0 -> []
    1 -> [range]
    n -> 
      let mid = start + n `div` 2
      in [(start, mid-1), (mid, end)]

prop_splitRange_preserves_rangeWidth :: (Int, Int) -> Bool
prop_splitRange_preserves_rangeWidth (a,b) = 
  let range = (min a b, max a b)
  in sum (map rangeWidth (splitRange range)) == rangeWidth range

data Box = Box 
  { xRange :: Range
  , yRange :: Range
  } deriving Show

boxSize :: Box -> Int
boxSize box = rangeWidth box.xRange * rangeWidth box.yRange

corners :: Box -> [Pos]
corners (Box (min_x, max_x) (min_y, max_y)) = 
  let
    top_left  = (min_x, min_y)
    top_right = (max_x, min_y)
    bot_left  = (min_x, max_y)
    bot_right = (max_x, max_y)
  in
    [top_left, top_right, bot_left, bot_right]

splitVertical :: Box -> [Box]
splitVertical box = do
  range <- splitRange box.xRange
  return $ box { xRange = range }

splitHorizontal :: Box -> [Box]
splitHorizontal box = do
  range <- splitRange box.yRange
  return $ box { yRange = range }

splitLongSide :: Box -> [Box]
splitLongSide box = 
  if rangeWidth box.xRange > rangeWidth box.yRange then
    splitVertical box
  else
    splitHorizontal box

splitReachable :: (Pos -> Bool) -> Box -> [Box]
splitReachable can_reach box =
  if boxSize box == 0 then 
    []
  else
    case map can_reach $ corners box of
      [ True , True , True , True  ] -> [box]
      [ True , True , False, False ] -> splitReachable can_reach =<< splitHorizontal box
      [ False, False, True , True  ] -> splitReachable can_reach =<< splitHorizontal box
      [ False, True , False, True  ] -> splitReachable can_reach =<< splitVertical box
      [ True , False, True , False ] -> splitReachable can_reach =<< splitVertical box
      _ | boxSize box == 1           -> []
        | otherwise                  -> splitReachable can_reach =<< splitLongSide box

main :: IO ()
main = do 
  poses <- parseWith parser "input/06.txt"

  putStr "Part 1: "
  print $ maximum $ do 
    (pos, other_poses) <- select poses
    case regionSize pos other_poses of
      Infinite    -> []
      Finite size -> [size]

  putStr "Part 2: "
  let in_reach :: Pos -> Bool
      in_reach pos = sum (map (dist pos) poses) < 10_000

      xs = map fst poses
      ys = map snd poses

      bounding_box = Box 
        (minimum xs, maximum xs) 
        (minimum ys, maximum ys)
  print 
    $ sum
    $ map boxSize
    $ splitReachable in_reach bounding_box
