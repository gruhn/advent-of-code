module Main where

hasDuplicate :: Eq a => [a] -> Bool
hasDuplicate [] = False
hasDuplicate (a:as) = 
  a `elem` as || hasDuplicate as

windows :: Int -> [a] -> [[a]]
windows size as
  | size > length as = []
  | otherwise = take size as : windows size (tail as)

main :: IO ()
main = do
  input <- readFile "input/06.txt"

  putStr "Part 1: "
  print $ (+4) $ length $ takeWhile hasDuplicate $ windows 4 input

  putStr "Part 2: "
  print $ (+14) $ length $ takeWhile hasDuplicate $ windows 14 input