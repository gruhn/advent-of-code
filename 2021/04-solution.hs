#!/usr/bin/env stack
-- stack --resolver lts-18.18 script
{-# LANGUAGE TupleSections #-}
module Day04 where 
    
import Data.List (elemIndex, transpose, find, nub, partition, delete)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn sep list =
    case elemIndex sep list of 
        Just index -> take index list : splitOn sep (drop (index+1) list)
        Nothing    -> [list]

type Board = [[Int]]
type Bingo = (Int, Board)

parseDraws :: String -> [Int]
parseDraws = map read . splitOn ','

parseBoard :: [String] -> Board
parseBoard lines =
    let rows = map (map read . words) lines
        cols = transpose rows
    in rows ++ cols

parseBoards :: [String] -> [Board]
parseBoards = map parseBoard . splitOn "" 

parseInput :: String -> ([Int], [Board])
parseInput input =
    let (draws:_newline:boards) = lines input
    in (parseDraws draws, parseBoards boards)

markBoard :: Int -> Board -> Board
markBoard draw = map (delete draw)

isBingo :: Board -> Bool
isBingo = elem []

play :: [Int] -> [Board] -> [Bingo]
play [] _ = []
play (draw:draws) boards = 
    let boardsMarked = map (markBoard draw) boards
        (bingoBoards, noBingoBoards) = partition isBingo boardsMarked
        bingos = map (draw,) bingoBoards
    in bingos ++ play draws noBingoBoards

bingoScore :: Bingo -> Int
bingoScore (lastDraw, board) = 
    (sum . nub . concat $ board) * lastDraw

main :: IO ()
main = do
    input <- readFile "04-input.txt"
    let (draws, boards) = parseInput input
    let bingosInOrder = play draws boards
    putStrLn "Part 1:"
    print $ bingoScore . head $ bingosInOrder
    putStrLn "Part 2:"
    print $ bingoScore . last $ bingosInOrder