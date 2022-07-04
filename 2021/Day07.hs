module Main where
import ParseUtil (splitOn)

parseInput :: String -> [Int]
parseInput = map read . splitOn ","

main :: IO ()
main = do
    input <- parseInput <$> readFile "2021/07-input.txt"
    putStr "Part 1: "
    print $ minimize cost1 input
    putStr "Part 2: "
    print $ minimize cost2 input

cost1 :: Int -> Int -> Int
cost1 pos x = abs (x - pos)

cost2 :: Int -> Int -> Int
cost2 pos x = 
    let n = cost1 pos x
    in (n^2 + n) `div` 2

minimize :: (Int -> Int -> Int) -> [Int] -> Int
minimize cost xs =
    let min = minimum xs
        max = maximum xs
        totalCost pos = sum $ map (cost pos) xs
    in minimum $ map totalCost [min..max] 