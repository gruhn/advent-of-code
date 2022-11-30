module Main where

import Data.List (sort)

data Line = Correct | Incorrect Char | Incomplete [Char]
    deriving Show

isIncorrect :: Line -> Bool
isIncorrect (Incorrect _) = True
isIncorrect _             = False

isIncomplete :: Line -> Bool
isIncomplete (Incomplete _) = True
isIncomplete _              = False

middle :: [a] -> a
middle list = 
    let mid = length list `div` 2
    in list !! mid

main :: IO ()
main = do
    parsed <- map parseLine . lines <$> readFile "input/10.txt"
    putStr "Part 1: "
    print $ sum . map incorrectCharScore . filter isIncorrect $ parsed
    putStr "Part 2: "
    print $ middle . sort . map incompleteScore . filter isIncomplete $ parsed

incorrectCharScore :: Line -> Int
incorrectCharScore (Incorrect char) = case char of
    ')' -> 3; ']' -> 57; '}' -> 1197; '>' -> 25137; _ -> 0
incorrectCharScore _ = 0

incompleteScore :: Line -> Int
incompleteScore (Incomplete chars) =
    foldl combine 0 $ map charScore chars
    where
        charScore char = case char of
            ')' -> 1; ']' -> 2; '}' -> 3; '>' -> 4; _ -> 0
        combine acc score = acc * 5 + score
incompleteScore _ = 0

parseLine :: String -> Line
parseLine chars = go chars []
    where 
        go [] [] = Correct
        go [] (d:ds) = Incomplete (d:ds)
        go ('(' : cs) ds = go cs (')' : ds)
        go ('[' : cs) ds = go cs (']' : ds)
        go ('{' : cs) ds = go cs ('}' : ds)
        go ('<' : cs) ds = go cs ('>' : ds)
        go (')' : cs) (')' : ds) = go cs ds
        go (']' : cs) (']' : ds) = go cs ds
        go ('}' : cs) ('}' : ds) = go cs ds
        go ('>' : cs) ('>' : ds) = go cs ds
        go (c : _) _ = Incorrect c
