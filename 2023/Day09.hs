{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Fuse foldr/map" #-}
module Main where
import Utils (Parser, parseFile, integer)
import Text.Megaparsec (sepEndBy, some)
import Text.Megaparsec.Char (newline)
import Control.Monad (guard)

parser :: Parser [[Int]]
parser = some integer `sepEndBy` newline

diffs :: [Int] -> [Int]
diffs xs = zipWith (-) (tail xs) xs

buildTriangle :: [Int] -> [[Int]]
buildTriangle = takeWhile (not . all (==0)) . iterate diffs

main :: IO ()
main = do
  input <- parseFile parser "input/09.txt"

  let triangles = map buildTriangle input

  putStr "Part 1: "
  let extrapolate_right = sum . map last
  print $ sum $ map extrapolate_right triangles

  putStr "Part 2: "
  let extrapolate_left = foldr (-) 0 . map head
  print $ sum $ map extrapolate_left triangles
