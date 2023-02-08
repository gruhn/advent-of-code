module Main where

import qualified ParseUtils as P
import Text.Megaparsec.Char ( char, string, newline )
import Text.Megaparsec ( sepBy )
import Data.Foldable ( maximumBy )
import Data.Function ( on )
import qualified Cuboid as C

type Vec = [Int]

parser :: P.Parser [(Vec, Int)]
parser = line `sepBy` newline
  where
    vec :: P.Parser Vec
    vec = string "pos=<" *> P.integer `sepBy` char ',' <* char '>'
      
    strength :: P.Parser Int
    strength = string ", r=" *> P.integer

    line :: P.Parser (Vec, Int)
    line = (,) <$> vec <*> strength
    
manhattanDist :: Vec -> Vec -> Int
manhattanDist v w = sum $ map abs $ zipWith (-) v w

boundingBox :: (Vec, Int) -> C.Cuboid
boundingBox (vec, reach) = C.Cuboid (map to_range vec)
  where
    to_range :: Int -> C.Range
    to_range x = C.Range (x - reach) (x + reach) 

main :: IO ()
main = do
  input <- P.parseHardError parser <$> readFile "input/23.txt"

  putStr "Part 1: "
  let (origin, reach) = maximumBy (compare `on` snd) input
  print 
    $ length
    $ filter ((<= reach) . manhattanDist origin)
    $ map fst input

  putStr "Part 2: "
