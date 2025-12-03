module Main (main) where
import Data.Char (digitToInt)
import Data.Foldable (foldl')

parse :: String -> [[Int]]
parse input = [ map digitToInt line | line <- lines input ]

digitsToDecimal :: [Int] -> Int
digitsToDecimal = foldl' (\acc digit -> 10*acc + digit) 0

pickDigits :: Int -> [Int] -> [Int]
pickDigits 0 _ = []
pickDigits digit_count digits =
  let
    (pre, suf) = splitAt (length digits - digit_count + 1) digits
    pre_rest = tail $ dropWhile (< maximum pre) pre
  in
    maximum pre : pickDigits (digit_count-1) (pre_rest ++ suf)

main :: IO ()
main = do
  input <- parse <$> readFile "input/03.txt"

  putStr "Part 1: "
  print $ sum $ map (digitsToDecimal . pickDigits 2) input

  putStr "Part 2: "
  print $ sum $  map (digitsToDecimal . pickDigits 12) input
