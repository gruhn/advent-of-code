module Main (main) where

import Utils (Parser, parseFile, countBy)
import Control.Applicative ((<|>))
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (newline, char)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.List (scanl', tails)

parser :: Parser [Int]
parser = (left_turn <|> right_turn) `sepEndBy` newline
  where
    left_turn :: Parser Int
    left_turn = char 'L' *> (negate <$> decimal)

    right_turn :: Parser Int
    right_turn = char 'R' *> decimal

divides :: Int -> Int -> Bool
divides a b = b `mod` a == 0

multiplesInRange :: Int -> Int -> Int -> Int
multiplesInRange mult range_start range_end =
  (range_end `div` mult) - ((range_start - 1) `div` mult)

main :: IO ()
main = do
  turns <- parseFile parser "input/01.txt"
  let partial_sums = scanl' (+) 50 turns

  putStr "Part 1: "
  print $ countBy (100 `divides`) partial_sums

  putStr "Part 2: "
  print $ sum $ do
    (curr:next:_) <- tails partial_sums
    if curr < next then
      return $ multiplesInRange 100 (curr+1) next
    else if curr > next then
      return $ multiplesInRange 100 next (curr-1)
    else
      return 0
