module Day10 (solver, parser) where

import Text.Parsec.String (Parser)
import Text.Parsec (newline, sepBy)
import qualified Data.List as List
import Utils (natural)
import Data.List.NonEmpty (groupBy)

parser :: Parser [Int]
parser = natural `sepBy` newline

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