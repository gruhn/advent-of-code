module Day15 (day15) where

import Algorithm.Search (dijkstra)
import Text.Parsec (many1, newline, sepBy)
import Text.Parsec.Char (digit)
import Data.Char (digitToInt, intToDigit)
import qualified Data.Map as Map
import Text.Parsec.String (Parser, parseFromFile)
import Data.Maybe (mapMaybe)
import Data.Either ()

type Cave = Map.Map (Int, Int) Int

fromGrid :: [[Int]] -> Cave
fromGrid grid =
    let go (x,y) [] = Map.empty
        go (x,y) ([]:rows) = go (0,y+1) rows
        go (x,y) ((cell:row):rows) =
            Map.insert (x,y) cell $ go (x+1,y) (row:rows)
    in go (0,0) grid

caveParser :: Parser Cave
caveParser = do
    charGrid <- many1 digit `sepBy` newline
    let intGrid = map (map digitToInt) charGrid
        cave = fromGrid intGrid
    return cave

type State = ((Int, Int), Int)

initialState :: State
initialState = ((0,0), 0)

finalState :: Cave -> State
finalState = Map.findMax

search :: Cave -> Maybe Int
search cave = 
    let cost _ (_, risk) = risk

        isFinalState = (finalState cave ==)

        decorateWithRisk :: (Int,Int) -> Maybe State
        decorateWithRisk key = do
            risk <- Map.lookup key cave
            return (key, risk)

        expandState :: State -> [State]
        expandState ((x,y), _) =
            let neighbors = [ (x-1, y), (x, y-1), (x+1, y), (x, y+1) ]
            in mapMaybe decorateWithRisk neighbors
    in do
        (totalRisk, path) <- dijkstra expandState cost isFinalState initialState

        return totalRisk

multiplyCave :: Int -> Cave -> Cave
multiplyCave factor cave =
    let (xn, yn) = fst $ finalState cave
        caveWidth = xn+1
        caveHeight = yn+1

        shiftPointBy (shiftX, shiftY) ((x,y), risk) =
            let newPos = (x + shiftX * caveWidth, y + shiftY * caveHeight)
                newRisk = (shiftX+shiftY+risk-1) `mod` 9 +1
            in (newPos, newRisk)

        shiftCaveBy shift
            = Map.fromList 
            $ map (shiftPointBy shift)
            $ Map.toList cave

    in Map.unions [ shiftCaveBy (x,y) | x <- [0..factor-1], y <- [0..factor-1] ]

day15 :: IO ()
day15 = do
    caveEither <- parseFromFile caveParser "15-input.txt"
    putStr "Part 1: "
    print $ search <$> caveEither
    putStr "Part 2: "
    print $ search . multiplyCave 5 <$> caveEither

showCave :: Cave -> String
showCave cave =
    let (xn, yn) = fst $ finalState cave
        showPoint xy = maybe ' ' intToDigit (Map.lookup xy cave)
        row y = [ showPoint (x,y) | x <- [0..xn] ]
    in unlines [ row y | y <- [0..yn]]