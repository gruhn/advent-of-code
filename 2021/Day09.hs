module Main where
import Data.Array
import Data.Char (digitToInt)
import Data.Maybe (mapMaybe)
import Data.List (sortBy)

listArrayInt :: [a] -> Array Int a
listArrayInt xs = listArray (0, length xs -1) xs

parseRow :: String -> Array Int Int
parseRow = listArrayInt . map digitToInt

type Grid = Array Int (Array Int Int)

parseGrid :: String -> Grid
parseGrid = listArrayInt . map parseRow . lines

main :: IO ()
main = do
    grid <- parseGrid <$> readFile "input/09.txt"
    putStr "Part 1: "
    print $ part1 grid
    putStr "Part 2: "
    print $ part2 grid

part1 :: Grid -> Maybe Int
part1 grid
    = fmap (sum . map (+1)) 
    . mapM (getAt grid) 
    $ lowPoints grid

part2 :: Grid -> Int
part2 grid
    = product . take 3 . sortBy (flip compare)
    . map (length . basinAround grid) 
    $ lowPoints grid

basinAround :: Grid -> (Int, Int) -> [(Int, Int)]
basinAround grid point = explore point []
    where 
        isBoundary :: (Int, Int) -> Bool
        isBoundary point = all (== 9) (getAt grid point)

        explore :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
        explore (x,y) basin
            | (x,y) `elem` basin = basin
            | isBoundary (x,y) = basin
            | otherwise = foldr explore ((x,y):basin) [ (x-1,y), (x+1,y), (x,y-1), (x,y+1) ]

lowPoints :: Grid -> [(Int, Int)]
lowPoints grid = filter (isLowPoint grid) . coords $ grid

isLowPoint :: Grid -> (Int, Int) -> Bool
isLowPoint grid (x,y) =
    case getAt grid (x,y) of
        Just value -> all (> value) (neighbors grid (x,y))
        Nothing    -> False

neighbors :: Grid -> (Int, Int) -> [Int]
neighbors grid (x,y) = 
    mapMaybe (getAt grid) [ (x-1,y), (x+1,y), (x,y-1), (x,y+1) ]

coords :: Grid -> [(Int, Int)]
coords grid = [ (x,y) | x <- [0..dimX grid], y <- [0..dimY grid] ]

getAt :: Grid -> (Int, Int) -> Maybe Int
getAt grid (x,y)
    | x >= 0 && y >= 0 && x <= dimX grid && y <= dimY grid = Just (grid ! x ! y) 
    | otherwise = Nothing

dimX :: Grid -> Int
dimX = snd . bounds

dimY :: Grid -> Int
dimY grid = snd $ bounds (grid ! 0)
    