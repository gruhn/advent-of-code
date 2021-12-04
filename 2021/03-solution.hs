#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

import Data.Char (digitToInt)

main :: IO ()
main = do
    input <- readFile "03-input.txt"
    let parsed = parseInput input
    putStrLn "Part 1:"
    print $ gammaRate parsed

type Bin = Int

binToDec :: [Int] -> Int
binToDec = binToDec' 0
    where
        binToDec' exp [] = 0
        binToDec' exp (x:xs) = x * 2^exp + binToDec' (exp+1) xs

parseInput :: String -> [Int]
parseInput = map (binToDec . map digitToInt) . lines

mostCommonDigit :: [Bin] -> Bin
mostCommonDigit 
    = (\x -> x+1 `div` 2) . signum . sum . map (\x -> x*2-1)
    
headDigit :: Int -> Bin
headDigit x = x `mod` 2

tailDigits :: Int -> Int
tailDigits x = (x - headDigit x) `div` 2

gammaRate :: [Int] -> [Bin]
gammaRate numbers = 
    let x  = mostCommonDigit $ map headDigit numbers
        xs = gammaRate $ map tailDigits numbers
    in x:xs