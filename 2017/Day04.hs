module Main where

import Data.List (sort)
import Data.Containers.ListUtils

noDuplicates :: [String] -> Bool
noDuplicates as = length as == length (nubOrd as)

main :: IO ()
main = do
  input <- map words . lines <$> readFile "input/04.txt"

  putStr "Part 1: "
  print $ length $ filter noDuplicates input

  putStr "Part 2: "  
  print $ length $ filter noDuplicates $ map (map sort) input
