module Main where

import Utils (Parser, parseFile)

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Control.Arrow (second, Arrow (..))
import Control.Monad (guard)
import Data.List (transpose)
import Data.Foldable (traverse_)

type Pattern = [String]

parser :: Parser [Pattern]
parser = pattern `sepEndBy` newline
  where
    pattern :: Parser Pattern
    pattern = some (line <* newline)

    line :: Parser String
    line = some (oneOf (".#" :: String))

card :: [Bool] -> Int
card = length . filter not

findMirror :: Pattern -> [Int]
findMirror pat = do
  index <- [1 .. length (head pat)-1]
  guard 
    $ (==1)
    $ sum
    $ map card
    $ map (uncurry (zipWith (==)))
    $ map (first reverse . splitAt index) pat

  return index

main :: IO ()
main = do
  input <- parseFile parser "input/13.txt"

  -- traverse_ (\str -> do traverse_ putStrLn str; print "") $ do 
  print $ sum $ do
    pat <- input
    let vert = findMirror pat
        hori = map (*100) $ findMirror (transpose pat)

    let index = head $ vert ++ hori
    return index

  -- putStr "Part 1: "
  -- putStr "Part 2: "
