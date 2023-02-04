module Main where

import Text.Megaparsec ( Parsec, sepBy )
import Text.Megaparsec.Char ( char, newline )
import Data.Void ( Void )
import ParseUtils ( Parser, integer, parseHardError )
import Data.List ( partition )

type Vec = [Int]

parser :: Parser [Vec]
parser = (integer `sepBy` char ',') `sepBy` newline

manhattanDist :: Vec -> Vec -> Int
manhattanDist v w = sum $ map abs $ zipWith (-) v w
    
cluster :: [Vec] -> [[Vec]]
cluster [] = []
cluster (centroid : vecs) = new_cluster : cluster rest_vecs
  where
    (new_cluster, rest_vecs) = collect [centroid] vecs

    collect :: [Vec] -> [Vec] -> ([Vec], [Vec])
    collect [] rest_vecs = ([], rest_vecs)
    collect (w : worklist) vecs = (w : inside_cluster, outside_cluster)
      where
        (more_worklist, rest_vecs) = partition ((<= 3) . manhattanDist w) vecs
        (inside_cluster, outside_cluster) = collect (worklist <> more_worklist) rest_vecs

main :: IO ()
main = do 
  input <- parseHardError parser <$> readFile "input/25.txt"

  putStr "Part 1: "
  print $ length $ cluster input
