module Main where

import Text.Megaparsec (parse, errorBundlePretty)
import qualified Data.Set as Set
import Data.Set (Set)

import GridParser (parser)

type Point = (Int,Int)
type Grid = Set.Set Point

neighbors :: Grid -> Point -> Int
neighbors grid (x,y) = length $
    filter (`Set.member` grid) 
        [ (x+1,y-1), (x+1,y), (x+1,y+1)
        , (x,y+1), (x-1,y+1), (x-1,y)
        , (x-1,y-1), (x,y-1) ]

step1 :: (Int,Int) -> Grid -> Grid
step1 (width,height) grid = Set.fromList $
    let rangeX = [0..width-1]
        rangeY = [0..height-1]
        
        isOn (x,y) =
            if Set.member (x,y) grid then 
                neighbors grid (x,y) `elem` [2,3]
            else 
                neighbors grid (x,y) == 3

    in  [ (x,y) | x <- rangeX, y <- rangeY, isOn (x,y) ]

step2 :: (Int,Int) -> Grid -> Grid
step2 (width,height) grid =
    let grid' = step1 (width,height) grid
        corners = Set.fromList 
            [ (0,0),(0,height-1),(width-1,0),(width-1,height-1) ]
    in  Set.union grid' corners

main :: IO ()
main = do
    input <- parse parser "" <$> readFile "2015/input/18.txt"
    case input of
        Left error -> putStr (errorBundlePretty error)
        Right pointList -> do
            let points = Set.fromList pointList

            putStr "Part 1: "
            print $ Set.size $ (!! 100)
                  $ iterate (step1 (100,100)) points

            putStr "Part 2: "
            print $ Set.size $ (!! 100)
                  $ iterate (step2 (100,100)) points