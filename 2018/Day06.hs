module Main where

import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (string, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import ParseUtils (Parser, parseWith)
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Applicative ((<|>))
import Data.Foldable (minimumBy)
import Data.Function (on)
import Debug.Trace 
import Control.Monad (guard)

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

select :: [a] -> [(a,[a])]
select []     = []
select (a:as) = (a,as) : [ (a',a:as') | (a',as') <- select as ]

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
  print $ _ 
