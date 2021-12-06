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
    print $ map (descendantCount 8) ages
    putStr "Part 2: "
    print $ map (descendantCount' 8) ages

descendantCount :: Int -> Int -> Int
descendantCount 0 age = 1
descendantCount days 0 = descendantCount (days-1) 6 + descendantCount (days-1) 8
descendantCount days age = descendantCount (days-1) (age-1)

binom :: Int -> Int -> Int
binom n k 
    | k < 0     = 0
    | k > n     = 0
    | otherwise = product [(n-k+1)..n] `div` product [1..k]

maxGenerations :: Int -> Int
maxGenerations days = days `div` 7

descendantCount' :: Int -> Int -> Int
descendantCount' days age 
    = sum 
    . map (\(n,k) -> binom (n+k) k) 
    $ map (\x -> (x, ((days+6-age) - 7*x) `div` 9)) [0..maxGenerations (days+6-age)]