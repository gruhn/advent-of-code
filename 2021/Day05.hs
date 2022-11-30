module Main where 

import ParseUtil (splitOn)
import qualified Data.Map as Map

type Point = (Int, Int)
type Grid = Map.Map Point Int
data Line = Line Point Point

parseInput :: String -> Maybe [Line]
parseInput = mapM parseLine . lines

parseLine :: String -> Maybe Line
parseLine str = 
    case map (map read . splitOn ",") $ splitOn " -> " str of
        [[x1,y1], [x2,y2]] -> Just (Line (x1, y1) (x2, y2))
        _                  -> Nothing

points :: Line -> [Point]
points (Line start@(x1, y1) end@(x2, y2)) = 
    takeWhile (/= end) (iterate step start) ++ [end]
    where
        stepX = signum (x2 - x1)
        stepY = signum (y2 - y1)
        step (x,y) = (x+stepX, y+stepY)

updateGrid :: Grid -> Line -> Grid
updateGrid grid line = foldl incrementCell grid (points line)
    where
        incrementCell grid point = Map.insertWith (+) point 1 grid
    
countOverlaps :: [Line] -> Int
countOverlaps = 
    Map.size . Map.filter (>1) . foldl updateGrid Map.empty

notDiagonal :: Line -> Bool
notDiagonal (Line (x1, y1) (x2, y2)) = x1 == x2 || y1 == y2

main :: IO ()
main = do
    input <- readFile "input/05.txt"
    let lines = parseInput input
    putStr "Part 1: "
    print $ fmap (countOverlaps . filter notDiagonal) lines
    putStr "Part 2: "
    print $ fmap countOverlaps lines