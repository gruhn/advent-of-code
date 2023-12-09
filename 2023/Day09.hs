module Main where

import Utils (Parser, parseFile, integer)
import Text.Megaparsec (sepEndBy, some)
import Text.Megaparsec.Char (newline)
import Control.Monad (guard)
import Data.Foldable (traverse_)

parser :: Parser [[Int]]
parser = line `sepEndBy` newline
  where
    line :: Parser [Int]
    line = some integer 

extrapolateRight :: [Int] -> [Int]
extrapolateRight seq
  | all (==0) seq = seq
  | otherwise     = seq ++ [right]
  where
    right = left + low
    left = last seq
    low = last $ extrapolateRight $ zipWith (-) (tail seq) seq

extrapolateLeft :: [Int] -> [Int]
extrapolateLeft seq
  | all (==0) seq = seq
  | otherwise     = left : seq
  where
    left = right - low
    right = head seq
    low = head $ extrapolateLeft $ zipWith (-) (tail seq) seq

main :: IO ()
main = do
  input <- parseFile parser "input/09.txt"

  putStr "Part 1: "
  print $ sum $ map (last . extrapolateRight) input

  putStr "Part 2: "
  print $ sum $ map (head . extrapolateLeft) input
