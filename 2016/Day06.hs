module Main where
import Data.List (group, sort, maximumBy, transpose, minimumBy)
import Control.Arrow ((&&&))
import Data.Function (on)

itemsWithFrequency :: Ord a => [a] -> [(Int, a)]
itemsWithFrequency = 
  sort . fmap (length &&& head) . group . sort

main :: IO ()
main = do
  rows <- lines <$> readFile "input/06.txt"
  let columns = transpose rows

  putStr "Part 1: "
  print $ snd . head . itemsWithFrequency <$> columns

  putStr "Part 2: "
  print $ snd . last . itemsWithFrequency <$> columns