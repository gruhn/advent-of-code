module Main (main) where

import Utils (parseFile, Parser, integer, countBy)
import Text.Megaparsec.Char (newline)
import Control.Applicative (some)
import Text.Megaparsec (sepEndBy)

parser :: Parser [[Int]]
parser = some integer `sepEndBy` newline

deletes :: [Int] -> [[Int]]
deletes []     = []
deletes (x:xs) = xs : map (x:) (deletes xs)

safe :: [Int] -> Bool
safe nums = all increasing adjacent_diffs || all decreasing adjacent_diffs
  where
    adjacent_diffs = zipWith (-) nums (tail nums) 
    increasing x =  1 <= x && x <=  3
    decreasing x = -3 <= x && x <= -1

main :: IO ()
main = do
  input <- parseFile parser "input/02.txt"

  putStr "Part 1: "
  print $ countBy safe input

  putStr "Part 2: "
  print $ countBy (any safe . deletes) input
