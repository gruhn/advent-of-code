{-# LANGUAGE TupleSections #-}
module Main where 
import Data.List (elemIndex, transpose, find, nub, partition, delete)
import ParseUtil (splitOn)

type Board = [[Int]]
type Bingo = (Int, Board)

parseDraws :: String -> [Int]
parseDraws = map read . splitOn ","

parseBoard :: [String] -> Board
parseBoard lines =
    let rows = map (map read . words) lines
        cols = transpose rows
    in rows ++ cols

parseBoards :: [String] -> [Board]
parseBoards = map parseBoard . splitOn [""]

parseInput :: String -> ([Int], [Board])
parseInput input =
    let (draws:_newline:boards) = lines input
    in (parseDraws draws, parseBoards boards)

markBoard :: Int -> Board -> Board
markBoard draw = map (delete draw)

isBingo :: Board -> Bool
isBingo = elem []

bingosInOrder :: [Int] -> [Board] -> [Bingo]
bingosInOrder [] _ = []
bingosInOrder (draw:draws) boards = 
    let boardsMarked = map (markBoard draw) boards
        (bingoBoards, noBingoBoards) = partition isBingo boardsMarked
        bingos = map (draw,) bingoBoards
    in bingos ++ bingosInOrder draws noBingoBoards

bingoScore :: Bingo -> Int
bingoScore (lastDraw, board) = 
    (sum . nub . concat $ board) * lastDraw

main :: IO ()
main = do
    input <- readFile "input/04.txt"
    let (draws, boards) = parseInput input
    let bingos = bingosInOrder draws boards
    putStr "Part 1:"
    print $ bingoScore (head bingos)
    putStr "Part 2:"
    print $ bingoScore (last bingos)