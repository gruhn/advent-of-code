module Main where
import Data.Function ( (&), on )
import Data.List (sortBy, sort, (\\), minimumBy, find)

input :: [Int]
input = 
    [ 1, 3, 5, 11, 13, 17, 19, 23
    , 29, 31, 37, 41, 43, 47, 53
    , 59, 67, 71, 73, 79, 83, 89
    , 97, 101, 103, 107, 109, 113 ]

subsets :: [a] -> [[a]]
subsets as =
    let go :: [a] -> Int -> [[a]]
        go _ 0 = [[]]
        go [] _ = []
        go (a:as) n = 
            ((a :) <$> go as (n-1)) ++ go as n
    in  concatMap (go as) [1 .. length as]

subsetsOfSum :: Int -> [Int] -> [[Int]]
subsetsOfSum s = filter ((s ==) . sum) . subsets

partitions :: Int -> [Int] -> [[[Int]]]
partitions 0 xs = [[]]
partitions n xs =
    let innerSum = sum xs `div` n
        firstSets = subsetsOfSum innerSum xs
        go p1 = (p1 :) <$> partitions (n-1) (xs \\ p1)
    in  concatMap go firstSets

firstGroupsFrom :: Int -> [Int] -> [[Int]]
firstGroupsFrom innerSum xs =
    let candidates0 = subsetsOfSum innerSum xs
        minLength = length (head candidates0)
        candidates1 = candidates0
            & takeWhile ((minLength ==) . length)
            & sortBy (compare `on` product)
    in  candidates1

main :: IO ()
main = do
    putStr "Part 1: "
    print
        $ fmap product
        $ find (\g -> not . null $ partitions 2 (input \\ g))
        $ firstGroupsFrom (sum input `div` 3) input

    putStr "Part 2: "
    print
        $ fmap product
        $ find (\g -> not . null $ partitions 3 (input \\ g))
        $ firstGroupsFrom (sum input `div` 4) input