module Main where

import Utils (Parser, parseFile, symbol, lexeme)
import Control.Monad (guard)
import Control.Applicative (many)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (newline)

parser :: Parser ([Int], [Int])
parser = do
  symbol "Time:"
  times <- many (lexeme decimal) <* newline
  symbol "Distance:"
  dists <- many (lexeme decimal) <* newline
  return (times, dists)

{-|

  Apply quadratic formula:

        -b +/- sqrt( b^2 - 4ac )
    x = -----------------------
                2a

  to solve:

    ax^2 + bx + c = 0

-}
solveQuadratic :: Double -> Double -> Double -> [Double]
solveQuadratic a b c = do
  sign <- [-1,1]
  let x = (-b + sign * sqrt (b^2 - 4*a*c)) / (2*a)
  -- would imply complex solution:
  guard $ not $ isNaN x 
  return x

winningStartTimes :: Int -> Int -> Int
winningStartTimes end_time record_dist = 
  let -- solve for x: -x^2 + end_time * x - record_dist = 0
      roots = solveQuadratic (-1) (fromIntegral end_time) (- fromIntegral record_dist)

      earliest_time = minimum roots
      latest_time = maximum roots

   in floor latest_time - ceiling earliest_time + 1

main :: IO ()
main = do
  (times, dists) <- parseFile parser "input/06.txt"

  putStr "Part 1: "
  print $ product $ zipWith winningStartTimes times dists  

  let concat_time :: Int
      concat_time = read $ concatMap show times

      concat_dist :: Int
      concat_dist = read $ concatMap show dists 

  putStr "Part 2: "
  print $ winningStartTimes concat_time concat_dist
