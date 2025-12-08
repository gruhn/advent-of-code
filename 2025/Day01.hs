module Main (main) where
import Data.List (scanl', tails)

parse :: String -> [Int]
parse input = do
  line <- lines input
  return $ case line of
    ('R':rest) -> read rest
    ('L':rest) -> read rest * (-1)
    _ -> error ("parse error: " ++ line)

divides :: Int -> Int -> Bool
divides a b = b `mod` a == 0

multiplesInRange :: Int -> Int -> Int -> Int
multiplesInRange mult range_start range_end =
  (range_end `div` mult) - ((range_start - 1) `div` mult)

main :: IO ()
main = do
  turns <- parse <$> readFile "input/01.txt"
  let partial_sums = scanl' (+) 50 turns

  putStr "Part 1: "
  print $ length $ filter (100 `divides`) partial_sums

  putStr "Part 2: "
  print $ sum $ do
    (curr:next:_) <- tails partial_sums
    if curr < next then
      return $ multiplesInRange 100 (curr+1) next
    else if curr > next then
      return $ multiplesInRange 100 next (curr-1)
    else
      return 0
