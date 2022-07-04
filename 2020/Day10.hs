module Main where

import qualified Data.List as List
import Data.List.NonEmpty (groupBy)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec (sepBy, Parsec, parse, errorBundlePretty)
import Text.Megaparsec.Char (newline)
import Data.Void (Void)
import Data.Either.Extra (fromEither)

parser :: Parsec Void String [Int]
parser = decimal `sepBy` newline

tribonacci :: [Integer]
tribonacci = go 0 1 1 where
    go x1 x2 x3 = x1 : go x2 x3 (x1+x2+x3)

solver :: [Int] -> IO ()
solver jolts = do
    let jolts' = [0] ++ List.sort jolts ++ [maximum jolts + 3]

    putStr "Part 1: "
    let diffs = zipWith (-) (tail jolts') jolts'
        n1 = length $ filter (==1) diffs
        n3 = length $ filter (==3) diffs
    print (n1 * n3)

    putStr "Part 2: "
    let runsOf1 = filter (elem 1) $ List.group diffs
        lengths = map ((+1) . length) runsOf1
        combinations = map (tribonacci !!) lengths
    print $ product combinations

main :: IO ()
main = do
    input <- parse parser "" <$> readFile "2020/input/10.txt"
    case input of 
        Left error -> putStr (errorBundlePretty error)
        Right parsed -> solver parsed