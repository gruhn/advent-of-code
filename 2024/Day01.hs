module Main (main) where

import Utils ( Parser, parseFile, integer, count )
import Text.Megaparsec.Char (newline)
import Text.Megaparsec (sepEndBy)
import Data.List (sort)

parser :: Parser [(Int, Int)]
parser = line `sepEndBy` newline
  where
    line :: Parser (Int, Int)
    line = (,) <$> integer <*> integer

main :: IO ()
main = do
  (left_list, right_list) <- unzip <$> parseFile parser "input/01.txt"

  putStr "Part 1: "
  let distance left_num right_num = abs (left_num - right_num)
  print $ sum $ zipWith distance (sort left_list) (sort right_list)

  putStr "Part 2: "
  let score left_num = left_num * count left_num right_list
  print $ sum $ map score left_list
