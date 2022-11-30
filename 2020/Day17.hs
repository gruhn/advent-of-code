module Main where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.List as List

type Point = [Int]

withCoordinates :: [[a]] -> [([Int],a)]
withCoordinates = go 0 0 where
    go x y [] = []
    go x y ([]:aas) = 
        go 0 (y+1) aas
    go x y ((a:as):aas) =
        ([x,y],a) : go (x+1) y (as:aas)

shifts :: Int -> [[Int]]
shifts 0 = [[]]
shifts n = 
    concatMap (\x -> map (x:) $ shifts (n-1)) [-1,0,1]

neighbors :: Point -> Set Point
neighbors p = 
    let dim = length p
    in Set.fromList 
        $ map (zipWith (+) p) 
        $ shifts dim

isActive :: Set Point -> Point -> Bool
isActive = flip Set.member

isActiveNext :: Set Point -> Point -> Bool
isActiveNext ps p =
    if isActive ps p 
        then activeNeighbors `List.elem` [2,3]
        else activeNeighbors == 3
    where
        activeNeighbors = length 
            $ Set.filter (isActive ps) 
            $ Set.delete p
            $ neighbors p

step :: Set Point -> Set Point
step ps
    = Set.filter (isActiveNext ps)
    $ Set.unions
    $ Set.map neighbors ps

steps :: Int -> Set Point -> Set Point
steps n ps = iterate step ps !! n

parse :: String -> Set Point
parse = Set.fromList
    . map fst
    . filter ((=='#') . snd) 
    . withCoordinates 
    . lines 

main :: IO ()
main = do
    input <- parse <$> readFile "input/17.txt"

    putStr "Part 1: "
    print $ length . steps 6 $ Set.map (++ [0]) input
        
    putStr "Part 2: "
    print $ length . steps 6 $ Set.map (++ [0,0]) input