module Main (main) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Function.Memoize (memoize3)

type Pos = (Int, Int)
type Keypad = Map Char Pos

numericKeypad :: Keypad
numericKeypad = Map.fromList 
  [ ('7', (0, 0)), ('8', (0, 1)), ('9', (0, 2))
  , ('4', (1, 0)), ('5', (1, 1)), ('6', (1, 2))
  , ('1', (2, 0)), ('2', (2, 1)), ('3', (2, 2))
  , ('0', (3, 1)), ('A', (3, 2))
  ]

directionalKeypad :: Keypad
directionalKeypad = Map.fromList
  [ ('^', (0, 1)), ('A', (0, 2))
  , ('<', (1, 0)), ('v', (1, 1)), ('>', (1, 2))
  ]

move :: Pos -> Char -> Pos
move (r, c) '^' = (r - 1, c)
move (r, c) 'v' = (r + 1, c)
move (r, c) '<' = (r, c - 1)
move (r, c) '>' = (r, c + 1)
move pos _ = pos

isValidPath :: Keypad -> Pos -> String -> Bool
isValidPath keypad start path = all (`elem` keypad) positions
  where
    positions = scanl move start path

getAllPaths :: Keypad -> Pos -> Pos -> [String]
getAllPaths keypad start end
  | start == end = ["A"]
  | otherwise =
      let (r1, c1) = start
          (r2, c2) = end
          vertical = replicate (abs (r2 - r1)) (if r2 > r1 then 'v' else '^')
          horizontal = replicate (abs (c2 - c1)) (if c2 > c1 then '>' else '<')
          path1 = vertical ++ horizontal ++ "A"
          path2 = horizontal ++ vertical ++ "A"
          validPaths = filter (isValidPath keypad start . init) [path1, path2]
      in if null validPaths then [path1] else validPaths

solveLength :: Int -> String -> Int
solveLength robotCount code = sum $ zipWith (solveMove robotCount) ('A' : code) code
  where
    solveMove = memoize3 go

    go :: Int -> Char -> Char -> Int
    go 0     _    _  = 1
    go depth from to =
      let
        fromPos = directionalKeypad Map.! from
        toPos = directionalKeypad Map.! to
        paths = getAllPaths directionalKeypad fromPos toPos
      in
        minimum $ map (solveLength (depth - 1)) paths

getNumericValue :: String -> Int
getNumericValue code = read $ filter (/= 'A') code

calculateComplexity :: String -> Int -> Int
calculateComplexity code robotCount = getNumericValue code * solveForCode robotCount code

solveForCode :: Int -> String -> Int
solveForCode robotCount code = sum $ map (uncurry (solveMove robotCount)) transitions
  where
    positions = map (numericKeypad Map.!) ('A' : code)
    transitions = zip positions (tail positions)

    solveMove d start end = minimum $ map (solveLength d) (getAllPaths numericKeypad start end)

main :: IO ()
main = do
  codes <- lines <$> readFile "input/21.txt"

  putStr "Part 1: "
  print $ sum $ map (`calculateComplexity` 2) codes

  putStr "Part 2: "
  print $ sum $ map (`calculateComplexity` 25) codes
