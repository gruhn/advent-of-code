{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map once" #-}
module Main where

import Utils (Parser, parseFile)

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Control.Arrow (second, Arrow (..))
import Control.Monad (guard)
import Data.List (transpose)
import Data.Foldable (traverse_)
import Data.Maybe (listToMaybe)

type Pattern = [String]

parser :: Parser [Pattern]
parser = pattern `sepEndBy` newline
  where
    pattern :: Parser Pattern
    pattern = some (line <* newline)

    line :: Parser String
    line = some (oneOf (".#" :: String))

countFalse :: [Bool] -> Int
countFalse = length . filter not

findMirror :: Int -> Pattern -> Maybe Int
findMirror error_count pat = go pat <|> fmap (*100) (go (transpose pat))
  where
    go :: Pattern -> Maybe Int
    go pat = listToMaybe $ do
      index <- [1 .. length (head pat)-1]
      guard
        $ (== error_count)
        $ sum
        $ map countFalse
        $ map (uncurry (zipWith (==)))
        $ map (first reverse . splitAt index) pat

      return index

main :: IO ()
main = do
  input <- parseFile parser "input/13.txt"

  putStr "Part 1: "
  print $ sum <$> traverse (findMirror 0) input

  putStr "Part 2: "
  print $ sum <$> traverse (findMirror 1) input

  -- putStr "Part 1: "
  -- putStr "Part 2: "
