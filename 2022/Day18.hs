module Main where

import Data.Set (Set)
import qualified Data.Set as S
import Utils (Parser, parseHardError)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (char, newline)
import Debug.Trace (trace)

type Point = (Int,Int,Int)

adjacent :: Point -> Set Point
adjacent (x,y,z) = S.fromList
  [ (x+1,y,z), (x-1,y,z)
  , (x,y+1,z), (x,y-1,z)
  , (x,y,z+1), (x,y,z-1) ]

parser :: Parser (Set Point)
parser = S.fromList <$>  points
  where
    point :: Parser Point
    point = (,,)
      <$> decimal <* char ','
      <*> decimal <* char ','
      <*> decimal

    points :: Parser [Point]
    points = point `sepBy` newline

data Axis = X | Y | Z

line :: Axis -> Point -> Set Point -> Set Point
line X (_,y,z) = S.filter (\(_,y',z') -> (y,z) == (y',z'))
line Y (x,_,z) = S.filter (\(x',_,z') -> (x,z) == (x',z'))
line Z (x,y,_) = S.filter (\(x',y',_) -> (x,y) == (x',y'))

isFree :: Set Point -> Point -> Bool
isFree points point = elem point $ do
  axis <- [X,Y,Z]
  let axis_points = S.insert point (line axis point points)
  [ maximum axis_points, minimum axis_points]

trappedPoints :: Set Point -> Set Point
trappedPoints points = final_trapped_points
  where
    surface_points = S.unions (S.map adjacent points) S.\\ points

    (initial_free_points, points_to_check) = S.partition (isFree points) surface_points

    (final_free_points, final_trapped_points) = foldr go (initial_free_points, S.empty) points_to_check

    grow :: Set Point -> Set Point
    grow pocket = pocket <> (S.unions (S.map adjacent pocket) S.\\ points)

    go :: Point -> (Set Point, Set Point) -> (Set Point, Set Point)
    go point (free_points, trapped_points) =
      case check_trapped (S.singleton point) of
        Left more_free_points -> (free_points <> more_free_points, trapped_points)
        Right trapped_pocket  -> (free_points, trapped_points <> trapped_pocket)
      where
        check_trapped :: Set Point -> Either (Set Point) (Set Point)
        check_trapped pocket
          | not (S.disjoint free_points pocket)    = Left pocket
          | not (S.disjoint trapped_points pocket) = Right pocket
          | pocket' == pocket                      = Right pocket
          | otherwise                              = check_trapped pocket'
          where
            pocket' = grow pocket

main :: IO ()
main = do
  input <- parseHardError parser <$> readFile "input/18.txt"

  let surface_points = (\p -> adjacent p S.\\ input) <$> S.toList input

  putStr "Part 1: "
  print $ sum $ S.size <$> surface_points

  putStr "Part 2: "
  let trapped_points = trappedPoints input
  print $ sum $ S.size . (S.\\ trapped_points) <$> surface_points