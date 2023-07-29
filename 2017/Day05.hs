module Main where

import Utils (Parser, parseFile, takeWhileJust)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (newline, hspace)
import Data.Sequence (Seq)
import qualified Data.Sequence as S

parser :: Parser (Seq Int)
parser = S.fromList <$> int_list
  where
    int = signed hspace decimal
    int_list = int `sepBy` newline

type State = Maybe (Int, Seq Int)

stepPart1 :: State -> State
stepPart1 state = do
  (pos, jumps) <- state
  offset <- S.lookup pos jumps
  return (pos+offset, S.adjust (+1) pos jumps)

stepPart2 :: State -> State
stepPart2 state = do
  (pos, jumps) <- state
  offset <- S.lookup pos jumps
  let y = if offset >= 3 then (-1) else 1
  return (pos+offset, S.adjust (+y) pos jumps)

main :: IO ()
main = do
  input <- parseFile parser "input/05.txt"

  let start_state = Just (0, input)

  putStr "Part 1: "
  print $ length $ tail $ takeWhileJust $ iterate stepPart1 start_state

  putStr "Part 2: "
  print $ length $ tail $ takeWhileJust $ iterate stepPart2 start_state
