module Main where

import qualified Data.Set as S
import qualified Data.Map as M

splitMiddle :: [a] -> [[a]]
splitMiddle as = [front, rear]
  where
    middle = length as `div` 2
    (front, rear) = splitAt middle as

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n as = 
  take n as : chunksOf n (drop n as)

intersection :: Ord a => [[a]] -> [a]
intersection []  = []
intersection ass =
  S.toList $ foldr1 S.intersection $ S.fromList <$> ass

priority :: Char -> Int
priority c = dict M.! c
  where
    chars = ['a'..'z'] <> ['A'..'Z']
    dict = M.fromList $ zip chars [1 ..]

totalPriority :: [[String]] -> Int
totalPriority = sum . fmap (priority . head . intersection)

main :: IO ()
main = do 
  input <- lines <$> readFile "input/03.txt"
  
  putStr "Part 1: "
  print $ totalPriority $ splitMiddle <$> input

  putStr "Part 2: "
  print $ totalPriority $ chunksOf 3 input
