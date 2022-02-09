#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Day06 where
import ParseUtil (splitOn)

parseInput :: String -> [Int]
parseInput = map read . splitOn ","

main :: IO ()
main = do
    input <- readFile "06-input.txt"
    let ages = parseInput input
    putStr "Part 1: "
    print $ sum $ map (descendantCount 80) input
    putStr "Part 2: "
    print $ sum $ map (descendantCount 256) input
  
binom :: Integer -> Integer -> Integer
binom n k 
    | k < 0     = 0
    | k > n     = 0
    | otherwise = product [(n-k+1)..n] `div` product [1..k]
    
descendantCount :: Integer -> Integer -> Integer
descendantCount days age =
  let -- remaining days adjusted for age:
      days' = days - (age + 1)
      -- maximum number of 8 day periods in ancestor chain:
      max8 = days' `div` (8+1)
      -- maximum number of 6 day periods in ancestor chain given fixed number of 8s:
      max6 n = (days' - (8+1)*n) `div` (6+1)
  in 1 + sum [ binom (n+m) n | n <- [0 .. max8], m <- [0 .. max6 n] ]

