module Main where
import Data.List (group)

step :: [Int] -> [Int]
step = concatMap (\xs -> [length xs, head xs]) . group

lookAndSay :: Int -> [Int] -> Int
lookAndSay n = length . (!! n) . iterate step

main :: IO ()
main = do
    let input = [1,1,1,3,1,2,2,1,1,3] 

    putStr "Part 1: "
    print $ lookAndSay 40 input

    putStr "Part 2: "
    print $ lookAndSay 50 input