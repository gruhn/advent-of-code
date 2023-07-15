module Main where

import Utils (Parser, parseFile)
import Text.Megaparsec (sepBy, some)
import Text.Megaparsec.Char (hspace, newline, char, hspace1)
import Text.Megaparsec.Char.Lexer (decimal)
import Control.Monad

parser :: Parser [[Int]]
parser = row `sepBy` newline
  where
    row = decimal `sepBy` hspace1

checksum1 :: [Int] -> Int
checksum1 xs = maximum xs - minimum xs

checksum2 :: [Int] -> Int
checksum2 xs = head $ do
  x1 <- xs
  x2 <- xs
  guard (x1 /= x2)
  guard (x1 `mod` x2 == 0)
  return (x1 `div` x2)

main :: IO ()
main = do 
  input <- parseFile parser "input/02.txt"

  putStr "Part 1: "
  print $ sum $ map checksum1 input

  putStr "Part 2: "
  print $ sum $ map checksum2 input
