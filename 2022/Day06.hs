module Main where
import Data.List (findIndex, tails, nub)

allDistinct :: Eq a => [a] -> Bool
allDistinct as = nub as == as

windows :: Int -> [a] -> [[a]]
windows size as = take size <$> tails as

main :: IO ()
main = do
  input <- readFile "input/06.txt"

  putStr "Part 1: "
  print $ (+4) <$> findIndex allDistinct (windows 4 input)

  putStr "Part 2: "
  print $ (+14) <$> findIndex allDistinct (windows 14 input)