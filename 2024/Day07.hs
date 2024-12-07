module Main (main) where

import Utils (Parser, parseFile, integer)
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (newline, string)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Prelude hiding ((||))
import Control.Monad.Combinators.NonEmpty (some)

type Equation = (Int, NonEmpty Int)

parser :: Parser [Equation]
parser = equation `sepEndBy` newline
  where
    equation :: Parser Equation
    equation = (,) <$> integer <* string ": " <*> some integer

type Operator = Int -> Int -> Int

(||) :: Operator
(||) a b = read $ show a ++ show b

hasSolution :: [Operator] -> Equation -> Bool
hasSolution operators (result, first_operand :| rest_operands) =
  let
    check :: [Int] -> Int -> Bool
    check []                   temp = temp == result
    check (operand : operands) temp  
      | temp > result = False
      | otherwise = any (check operands) [ temp `op` operand | op <- operators ]
  in
    check rest_operands first_operand 

main :: IO ()
main = do
  input <- parseFile parser "input/07.txt"

  putStr "Part 1: "
  print $ sum $ map fst $ filter (hasSolution [(+), (*)]) input

  putStr "Part 2: "
  print $ sum $ map fst $ filter (hasSolution [(+), (*), (||)]) input
