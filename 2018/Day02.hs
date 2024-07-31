module Main where
import Data.List (group, sort, tails)
import Data.Containers.ListUtils (nubInt)
import Control.Monad (guard)

occurCount :: Ord a => [a] -> [Int]
occurCount = map length . group . sort

commonChars :: String -> String -> String
commonChars str1 str2 = do
  (char1, char2) <- zip str1 str2
  guard $ char1 == char2
  return char1

main :: IO ()
main = do
  input <- lines <$> readFile "input/02.txt"

  putStr "Part 1: "
  print $ product $ occurCount $ do
    line <- input
    char_count <- nubInt $ occurCount line
    guard $ char_count `elem` [2,3]
    return char_count

  putStr "Part 2: "
  print $ do
    line1:other_lines <- tails input
    line2 <- other_lines
    let common_chars = commonChars line1 line2
    guard $ length common_chars == length line1 - 1
    return common_chars
