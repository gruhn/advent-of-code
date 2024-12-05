module Main (main) where

import Text.Megaparsec
import Utils (Parser, parseFile, integer)
import Text.Megaparsec.Char (char, newline)
import Control.Monad (guard)
import Data.List (sortBy)
import qualified Data.Set as Set
import Data.Set (Set)

type OrderRule = (Int, Int)

parser :: Parser (Set OrderRule, [[Int]])
parser = (,) <$> order_rules <* newline <*> updates_list
  where
    order_rule :: Parser OrderRule
    order_rule = (,) <$> integer <* char '|' <*> integer

    order_rules :: Parser (Set OrderRule)
    order_rules = Set.fromList <$> order_rule `sepEndBy` newline

    updates :: Parser [Int]
    updates = integer `sepEndBy1` char ','

    updates_list :: Parser [[Int]]
    updates_list = updates `sepEndBy` newline

middle :: [Int] -> Int
middle xs
  | even (length xs) = error "even list has no middle"
  | otherwise = xs !! (length xs `div` 2)

compareUsing :: Set OrderRule -> Int -> Int -> Ordering
compareUsing rules a b
  | a == b                 = EQ
  | Set.member (a,b) rules = LT
  | Set.member (b,a) rules = GT
  | otherwise              = error "not a total order"

main :: IO ()
main = do
  (order_rules, updates_list) <- parseFile parser "input/05.txt"

  putStr "Part 1: "
  print $ sum $ do
    updates <- updates_list
    let updates_sorted = sortBy (compareUsing order_rules) updates
    guard $ updates == updates_sorted
    return $ middle updates

  putStr "Part 2: "
  print $ sum $ do
    updates <- updates_list
    let updates_sorted = sortBy (compareUsing order_rules) updates
    guard $ updates /= updates_sorted
    return $ middle updates_sorted
