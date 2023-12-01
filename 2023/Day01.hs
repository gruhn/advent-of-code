module Main where
import Data.Char (digitToInt, isDigit)
import Data.List (tails, isPrefixOf)
import Data.Maybe (mapMaybe)

parseLine1 :: String -> [Int]
parseLine1 = map digitToInt . filter isDigit

parseLine2 :: String -> [Int]
parseLine2 = mapMaybe to_digit . tails
  where
    to_digit :: String -> Maybe Int
    to_digit "" = Nothing
    to_digit str
      | isDigit (head str)       = Just $ digitToInt $ head str
      | "one"   `isPrefixOf` str = Just 1
      | "two"   `isPrefixOf` str = Just 2
      | "three" `isPrefixOf` str = Just 3
      | "four"  `isPrefixOf` str = Just 4
      | "five"  `isPrefixOf` str = Just 5
      | "six"   `isPrefixOf` str = Just 6
      | "seven" `isPrefixOf` str = Just 7
      | "eight" `isPrefixOf` str = Just 8
      | "nine"  `isPrefixOf` str = Just 9
      | otherwise = Nothing

score :: [[Int]] -> Int
score digits_per_line = sum $ do
  digits <- digits_per_line
  return $ head digits * 10 + last digits

main :: IO ()
main = do
  input_lines <- lines <$> readFile "input/01.txt"

  putStr "Part 1: "
  print $ score $ map parseLine1 input_lines

  putStr "Part 2: "
  print $ score $ map parseLine2 input_lines
