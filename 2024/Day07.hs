module Main (main) where

import Utils (Parser, parseFile, integer)
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (newline, string)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Prelude hiding ((||))
import Control.Monad.Combinators.NonEmpty (some)
import Control.Monad (guard)
import Data.Foldable (asum, toList)
import Data.Maybe (isJust)
import qualified Data.List.NonEmpty as NonEmpty

type Equation = (Int, NonEmpty Int)

parser :: Parser [Equation]
parser = equation `sepEndBy` newline
  where
    equation :: Parser Equation
    equation = (,) <$> integer <* string ": " <*> some integer

type Operator = Int -> Int -> Maybe Int

trySubtract :: Int -> Int -> Maybe Int
trySubtract a b = do
  guard $ a >= b
  return $ a - b

tryDivide :: Int -> Int -> Maybe Int
tryDivide a b = do
  guard $ a `mod` b == 0
  return $ a `div` b

tryStripSuffix :: Int -> Int -> Maybe Int
tryStripSuffix a b = do
  let digits = length $ show b
      (prefix, suffix) = divMod a (10 ^ digits)
  guard $ suffix == b
  return prefix

hasSolution :: [Operator] -> Equation -> Bool
hasSolution operators (result, operands) = 
  let
    check_reverse :: Int -> NonEmpty Int -> Maybe ()
    check_reverse result_rest (operand1 :| []) = 
      guard $ result_rest == operand1
    check_reverse result_rest (operand1 :| operand2 : rest_operands) = asum $ do
      op  <- operators
      res <- toList $ result_rest `op` operand1
      return $ check_reverse res (operand2 :| rest_operands)
  in
    isJust $ check_reverse result (NonEmpty.reverse operands)

main :: IO ()
main = do
  equations <- parseFile parser "input/07.txt"

  putStr "Part 1: "
  print $ sum $ map fst $ filter (hasSolution [trySubtract, tryDivide]) equations

  putStr "Part 2: "
  print $ sum $ map fst $ filter (hasSolution [trySubtract, tryDivide, tryStripSuffix]) equations
