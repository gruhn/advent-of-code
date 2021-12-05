
#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Day05 where 
import ParseUtil (splitOn)
import qualified Data.Map as Map

type Grid = Map.Map (Int, Int) Int
type Line = (Int, Int, Int, Int)

parseInput :: String -> Maybe [Line]
parseInput = mapM parseLine . lines

parseLine :: String -> Maybe Line
parseLine str = 
    case map (map read . splitOn ",") $ splitOn " -> " str of
        [[x1,y1], [x2,y2]] -> Just (x1, y1, x2, y2)
        _                  -> Nothing

pointsOf :: Line -> [(Int, Int)]
pointsOf (x1, y1, x2, y2) 
    | x1 == x2 && y1 == y2 = [(x1, y1)]
    | otherwise = (x1, y1) : pointsOf (x1+stepX, y1+stepY, x2, y2)  
    where
        stepX = signum (x2 - x1)
        stepY = signum (y2 - y1)

updateGrid :: Grid -> Line -> Grid
updateGrid grid line = 
    foldl (flip $ Map.alter updateCell) grid $ pointsOf line
    where 
        updateCell (Just count) = Just (count + 1)
        updateCell Nothing      = Just 1
    
countOverlaps :: [Line] -> Int
countOverlaps = Map.size . Map.filter (2<=) . foldl updateGrid Map.empty

notDiagonal :: Line -> Bool
notDiagonal (x1, y1, x2, y2) = x1 == x2 || y1 == y2

main :: IO ()
main = do
    input <- readFile "05-input.txt"
    let parsed = parseInput input
    putStrLn "Part 1:"
    print $ fmap (countOverlaps . filter notDiagonal) parsed
    putStrLn "Part 2:"
    print $ fmap countOverlaps parsed