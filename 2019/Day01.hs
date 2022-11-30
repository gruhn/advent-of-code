module Main where

import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec (sepBy, Parsec, parse, errorBundlePretty)
import Data.Void (Void)

parser :: Parsec Void String [Int]
parser = decimal `sepBy` newline

fuel :: Int -> Int
fuel mass = mass `div` 3 - 2

compoundFuel :: Int -> Int
compoundFuel =
    sum . takeWhile (>0) . tail . iterate fuel

main :: IO ()
main = do
    input <- parse parser "" <$> readFile "input/01.txt"

    putStr "Part 1: "
    print $ sum . map fuel <$> input

    putStr "Part 2: "
    print $ sum . map compoundFuel <$> input