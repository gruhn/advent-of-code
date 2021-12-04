#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

main :: IO ()
main = do
    input <- readFile "01-input.txt"
    let parsed = parseInput input
    putStrLn "Part 1:"
    print $ increaseCount parsed
    putStrLn "Part 2:"
    print $ windowedIncreaseCount parsed

parseInput :: String -> [Int]
parseInput = map read . lines

increaseCount :: [Int] -> Int
increaseCount (x1:x2:xs) 
    | x1 < x2   = increaseCount (x2:xs) + 1
    | otherwise = increaseCount (x2:xs)
increaseCount _ = 0

windowedIncreaseCount :: [Int] -> Int
windowedIncreaseCount (x1:x2:x3:x4:x5:xs)
    | x1+x2+x3 < x2+x3+x4 = windowedIncreaseCount (x2:x3:x4:x5:xs) + 1
    | otherwise           = windowedIncreaseCount (x2:x3:x4:x5:xs)
windowedIncreaseCount _ = 0