module Main (main) where

import Utils (parseFile, Parser)
import Text.Megaparsec (sepBy, optional)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Control.Monad (guard)

parser :: Parser [(Int,Int)]
parser = range `sepBy` char ',' <* optional newline
  where
    range :: Parser (Int,Int)
    range = do
      a <- decimal
      char '-'
      b <- decimal
      return (a,b)

isRepeated :: Int -> String -> Bool
isRepeated rep_count pattern =
  let
    (rep_length, rest_length) = length pattern `divMod` rep_count
    pattern_reconstructed = concat $ replicate rep_count $ take rep_length pattern
  in
    rest_length == 0 && pattern == pattern_reconstructed

main :: IO ()
main = do
  ranges <- parseFile parser "input/02.txt"

  putStr "Part 1: "
  print $ sum $ do
    (a,b) <- ranges
    value <- [a..b]
    let pattern = show value
    guard $ isRepeated 2 pattern
    return value

  putStr "Part 2: "
  print $ sum $ do
    (a,b) <- ranges
    value <- [a..b]
    let pattern = show value
    rep_count <- [1 .. length pattern `div` 2]
    guard $ isRepeated rep_count pattern
    return value
