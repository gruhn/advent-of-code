module Main where

import Utils (Parser, parseFile, takeWhileJust, integer)
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (newline)
import Data.Sequence (Seq)
import qualified Data.Sequence as S

parser :: Parser (Seq Int)
parser = S.fromList <$> integer `sepBy` newline

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
