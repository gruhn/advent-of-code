module Main where

import Text.Parsec (letter, many1, newline, sepBy, sepEndBy, endBy, many, choice, manyTill, count, oneOf)
import Text.Parsec.String (Parser, parseFromFile)
import qualified Data.Set as Set
import Data.Set (Set)

answersP :: Parser (Set Char)
answersP = Set.fromList <$> 
    many1 (oneOf ['a' .. 'z']) <* newline

groupP :: Parser [Set Char]
groupP = many1 answersP

groupListP :: Parser [[Set Char]]
groupListP = groupP `sepBy` newline

unions :: [[Set Char]] -> [Set Char]
unions = map Set.unions

intersections :: [[Set Char]] -> [Set Char]
intersections = map (foldr1 Set.intersection)

main :: IO ()
main = do
    groups <- parseFromFile groupListP "2020/input/06.txt"

    putStr "Part 1: "
    print $ sum . map Set.size . unions <$> groups

    putStr "Part 2: "
    print $ sum . map Set.size . intersections <$> groups
