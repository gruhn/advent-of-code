module Main where

import Data.Char (digitToInt, intToDigit)
import qualified Data.Map as Map
import Data.List (sort, groupBy)

parse :: String -> Grid Int
parse = fromNestedList . map (map digitToInt) . lines

main :: IO ()
main = do
    input <- parse <$> readFile "2021/11-input.txt"
    putStr "Part 1: "
    print $ sum $ map fst $ take 101 $ iterateStep input
    putStr "Part 2: "
    print $ length $ takeWhile (not . allFlash) $ iterateStep input

allFlash :: (Int, Grid Int) -> Bool
allFlash (flashCount, grid) = flashCount == Map.size grid

iterateStep :: Grid Int -> [(Int, Grid Int)]
iterateStep input = iterate (step . snd) (0, input)

step :: Grid Int -> (Int, Grid Int)
step = reset . propagate . incAll

incAll :: Grid Int -> Grid Int
incAll = Map.map (+1)

incNeighborsOf :: (Int, Int) -> Grid Int -> Grid Int
incNeighborsOf key grid =
    let neighbors = incAll $ neighborsOf key grid
    in Map.union neighbors grid

propagate :: Grid Int -> Grid Int
propagate grid =
    let (gt9, rest) = Map.partition (>9) grid
    in if Map.null gt9
        then grid
        else Map.union gt9 $ propagate $ foldr incNeighborsOf rest (Map.keys gt9)

reset :: Grid Int -> (Int, Grid Int)
reset grid = 
    let gt9 = Map.filter (>9) grid
    in (Map.size gt9, Map.unionWith (\_ _ -> 0) grid gt9)

-- Grid utils:

type Grid a = Map.Map (Int, Int) a

fromNestedList :: [[a]] -> Grid a 
fromNestedList = Map.fromList . coords 0 0 
    where 
        coords x y [] = []
        coords x y ([]:rows) = 
            coords 0 (y+1) rows
        coords x y ((val:vals):rows) = 
            ((x,y), val) : coords (x+1) y (vals:rows)

isNeighborOf :: (Int, Int) -> (Int, Int) -> Bool
isNeighborOf (x,y) key =
    key `elem` [ (x+dx, y+dy) | dx <- [-1,0,1], dy <- [-1,0,1] ]

neighborsOf :: (Int, Int) -> Grid a -> Grid a
neighborsOf (x,y) = 
    Map.filterWithKey (\key _ -> isNeighborOf (x,y) key)

showGrid :: Grid Int -> String
showGrid grid
    = unlines 
    . map (map (intToDigit . snd)) 
    . groupBy sameRow 
    . sort 
    . map reverseKey 
    $ Map.toList grid
    where
        sameRow ((y1, _), _) ((y2, _), _) = y1 == y2
        reverseKey ((x,y), val) = ((y,x), val)