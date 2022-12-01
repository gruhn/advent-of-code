module Main where
import Utils
import Text.Megaparsec (sepBy, chunk)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Control.Applicative (some)
import Data.List (sort)

parser :: Parser [[Int]]
parser = block `sepBy` newline
  where
    block = some (decimal <* newline)

main :: IO ()
main = do
  input <- parseHardError parser <$> readFile "input/01.txt"
  let input_sorted = reverse $ sort $ fmap sum input

  putStr "Part 1: "
  print $ head input_sorted 

  putStr "Part 2: "
  print $ sum $ take 3 input_sorted
