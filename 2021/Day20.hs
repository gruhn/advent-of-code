{-# LANGUAGE TupleSections #-}
module Main where

import Text.Parsec.String
import Text.Parsec
import qualified Data.Set as Set
import Data.Either (fromRight)

type Algorithm = Set.Set Int
data Plane = Plane
    { defaultValue :: Int
    , ones :: Set.Set (Int, Int) 
    , bounds :: (Int, Int, Int, Int) 
    }

oneCoords :: [[Int]] -> Set.Set (Int, Int)
oneCoords = Set.unions . zipWith pairEach [0..] . map oneIndices
    where
        pairEach y set = Set.map (,y) set

oneIndices :: [Int] -> Set.Set Int
oneIndices = Set.fromList
    . map fst
    . filter ((==1) . snd)
    . zip [0..] 

computeBounds :: Set.Set (Int, Int) -> (Int, Int, Int, Int)
computeBounds ones =
    let x0 = Set.findMin $ Set.map fst ones
        y0 = Set.findMin $ Set.map snd ones
        xn = Set.findMax $ Set.map fst ones
        yn = Set.findMax $ Set.map snd ones
    in (x0, y0, xn, yn)

pixel :: Parser Int
pixel = (char '.' >> return 0) <|> (char '#' >> return 1)

pixelRow :: Parser [Int]
pixelRow = many1 pixel

puzzleInput :: Parser (Algorithm, Plane)
puzzleInput = do
    algo <- oneIndices <$> pixelRow
    _ <- newline
    _ <- newline
    ones <- oneCoords <$> pixelRow `sepBy` newline
    return (algo, Plane 
        { defaultValue = 0
        , ones = ones
        , bounds = computeBounds ones 
        })

main :: IO ()
main = do
    inputRaw <- readFile "input/20.txt"
    let parsed = parse (puzzleInput <* eof) "" inputRaw
        result = do 
            (algo, plane) <- parsed
            return $ iterate (convolute algo) plane
    putStr "Part 1: "
    print $ Set.size . ones . (!! 2) <$> result
    putStr "Part 2: "
    print $ Set.size . ones . (!! 50) <$> result
    putStr
        $ fromRight "parse error" 
        $ showPlane . (!! 50) <$> result

valueAt :: Plane -> (Int, Int) -> Int
valueAt plane (x,y)
    | Set.member (x,y) (ones plane) = 1
    | inBounds plane (x,y)          = 0
    | otherwise                     = defaultValue plane

toDecimal :: [Int] -> Int
toDecimal [] = 0
toDecimal (x:xs) = x * 2 ^ length xs + toDecimal xs

convolution :: Algorithm -> Plane -> (Int, Int) -> Bool
convolution algo plane (x,y) = 
    let add (dx,dy) = (x+dx,y+dy)
        window = [ (-1,-1), (0,-1), (1,-1), (-1,0), (0,0), (1,0), (-1,1), (0,1), (1,1) ]
        binary = map (valueAt plane . add) window
        algoIndex = toDecimal binary
    in Set.member algoIndex algo

inBounds :: Plane -> (Int, Int) -> Bool
inBounds plane (x,y) =
    let (x0,y0,xn,yn) = bounds plane
    in x0 <= x && y0 <= y && x <= xn && y <= yn

convolute :: Algorithm -> Plane -> Plane
convolute algo plane =
    let (x0, y0, xn, yn) = bounds plane
        coords = [ (x,y) | x <- [x0-1..xn+1], y <- [y0-1..yn+1] ] 
        onlyOnes = Set.fromList $ filter (convolution algo plane) coords
    in Plane 
        { defaultValue = 1 - defaultValue plane
        , ones = onlyOnes
        , bounds = computeBounds onlyOnes
        }

showPlane :: Plane -> String
showPlane plane =
    let (x0, y0, xn, yn) = bounds plane
        showPoint xy
            | valueAt plane xy == 1 = '#'
            | otherwise             = '.'
        row y = [ showPoint (x, y) | x <- [x0-10..xn+10] ]
    in unlines [ row y | y <- [y0-10..yn+10]]