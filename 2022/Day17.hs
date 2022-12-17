{-# LANGUAGE OverloadedLists #-}
module Main where

import Data.Set (Set)
import qualified Data.Set as S
import Data.List (mapAccumL, scanl', unfoldr, group, partition, tails, isPrefixOf, findIndex)
import Data.Foldable (maximumBy, traverse_)
import Data.Function (on)
import Debug.Trace (trace)
import Control.Arrow (first)
import Data.Maybe (fromMaybe, fromJust)
import Utils (chunksOf, takeUntil)

type Vec = (Int,Int)

(.+) :: Vec -> Vec -> Vec
(.+) (x1, y1) (x2, y2) = (x1+x2, y1+y2)

toVec :: Char -> Vec
toVec '>' = (1,0)
toVec '<' = (-1,0)
toVec _ = undefined

step :: Set Vec -> [Vec] -> Set Vec -> ([Set Vec], [Vec])
step rocks = step_dir
  where
    step_dir :: [Vec] -> Set Vec -> ([Set Vec], [Vec])
    step_dir [] piece = ([piece], [])
    step_dir (dir:dirs) piece
      | out_of_bounds piece' = step_down dirs piece
      | colliding piece'     = step_down dirs piece
      | otherwise            = first (piece' :) $ step_down dirs piece'
      where
        piece' = S.map (.+ dir) piece

    step_down :: [Vec] -> Set Vec -> ([Set Vec], [Vec])
    step_down dirs piece
      | out_of_bounds piece' = ([piece], dirs)
      | colliding piece' = ([piece], dirs)
      | otherwise = first (piece' :) $ step_dir dirs piece'
      where
        piece' = S.map (.+ (0,-1)) piece

    colliding :: Set Vec -> Bool
    colliding piece = not $ S.disjoint piece rocks

    out_of_bounds :: Set Vec -> Bool
    out_of_bounds = any (\(x,y) -> x < 0 || x > 6 || y < 0)

pieces :: [Set Vec]
pieces = [minus, plus, rev_l, stick, square]
  where
    minus = [(0,0),(1,0),(2,0),(3,0)]
    plus = [(1,0),(0,1),(1,1),(2,1),(1,2)]
    rev_l = [(0,0),(1,0),(2,0),(2,1),(2,2)]
    stick = [(0,0),(0,1),(0,2),(0,3)]
    square = [(0,0),(1,0),(0,1),(1,1)]

towerHeight :: Set Vec -> Int
towerHeight rocks =
  fromMaybe 0 (S.lookupMax $ S.map snd rocks) - 1

run :: [Vec] -> [[Set Vec]]
run dirs = go S.empty (cycle dirs) (cycle pieces)
  where
    go :: Set Vec -> [Vec] -> [Set Vec] -> [[Set Vec]]
    go rocks dirs [] = []
    go rocks dirs (piece:pieces) = steps' : go (last steps') dirs' pieces
      where
        steps' = fmap (rocks <>) steps
        (steps, dirs') = step rocks dirs (adjust_new_piece rocks piece)

    adjust_new_piece :: Set Vec -> Set Vec -> Set Vec
    adjust_new_piece rocks = S.map (.+ base_pos)
      where
        ys = snd <$> S.toList rocks
        base_pos = (2, maximum (-1 : ys) + 4)

showGrid :: Set Vec -> String
showGrid rocks = unlines $ reverse rows
  where
    cell y x
      | (x,y) `S.member` rocks = '#'
      | otherwise = '.'

    row :: Int -> String
    row y = cell y <$> [0..6]

    rows = row <$> [0 .. towerHeight rocks]

main :: IO ()
main = do
  dirs <- fmap toVec <$> readFile "input/17.txt"

  let states = last <$> run dirs

  putStr "Part 1: "
  print $ towerHeight $ states !! 2022

  putStr "Part 2: "
  let heights = towerHeight <$> states
      height_diffs = zipWith (-) (tail heights) heights

      cycle_sep :: [Int]
      cycle_sep = [4,0,1,3,3,4,0,1,3,0]

      split_on_sep :: [Int] -> [Int] -> [[Int]]
      split_on_sep sep as = take (sep_index + length sep) as : split_on_sep sep (drop (sep_index + length sep) as)
        where
          sep_index = fromJust $ findIndex (isPrefixOf sep) (tails as)

      [a,b,c] = take 3 $ split_on_sep cycle_sep height_diffs

      factor = (1000000000000 - length a - length b) `div` length c
      remainder = (1000000000000 - length a - length b) `mod` length c

  print $ sum a + sum b + factor * sum c + sum (take remainder c)