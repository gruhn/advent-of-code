module Main where
import Data.List (delete)

input :: [Int]
input =
    [ 43,  3,  4, 10, 21, 44
    ,  4,  6, 47, 41, 34, 17
    , 17, 44, 36, 31, 46,  9
    , 27, 38 ]

sumTo :: Int -> [Int] -> [[Int]]
sumTo 0 [] = [[]]
sumTo _ [] = []
sumTo y (x:xs)
    | y < 0  = []
    | y == 0 = [[]]
    | otherwise =
        ((x:) <$> sumTo (y-x) xs) ++ sumTo y xs

main :: IO ()
main = do
    putStr "Part 1: "
    let combinations = sumTo 150 input
    print $ length combinations

    putStr "Part 2: "
    let hasMinLength xs = 
            length xs == minimum (length <$> combinations)
    print $ length $ filter hasMinLength combinations