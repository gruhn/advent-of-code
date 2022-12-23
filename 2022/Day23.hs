{-# LANGUAGE TupleSections #-}
module Main where
import Data.Set (Set)
import Utils (withCoordinates, converge)
import qualified Data.Map as M
import qualified Data.Set as S

type Point = (Int,Int)

parse :: String -> Set Point
parse str = M.keysSet $ M.filter (=='#') $ M.fromList $ withCoordinates $ lines str

-- If there is no Elf in the N, NE, or NW adjacent positions, the Elf proposes moving north one step.
-- If there is no Elf in the S, SE, or SW adjacent positions, the Elf proposes moving south one step.
-- If there is no Elf in the W, NW, or SW adjacent positions, the Elf proposes moving west one step.
-- If there is no Elf in the E, NE, or SE adjacent positions, the Elf proposes moving east one step.
adj :: Set Point -> Point -> [Bool]
adj ps (x,y) = fmap (`S.member` ps)
  [ (x-1,y-1), (x,y-1),(x+1,y-1)
  , (x-1,y),   (x,y),  (x+1,y)
  , (x-1,y+1), (x,y+1), (x+1,y+1) ]

go :: (Set Point, Int) -> (Set Point, Int)
go (points, i) = (should_not_move <> S.map (check ps') ps', (i+1) `mod` 4)
  where
    go0 :: Point -> (Point, Point)
    go0 (x,y) = ((x,y),) $ case adj points (x,y) of
      [ False, False, False, _, _, _, _, _, _ ] -> (x,y-1)
      [ _, _, _, _, _, _, False, False, False ] -> (x,y+1)
      [ False, _, _, False, _, _, False, _, _ ] -> (x-1,y)
      [ _, _, False, _, _, False, _, _, False ] -> (x+1,y)
      _ -> (x,y)

    go1 :: Point -> (Point, Point)
    go1 (x,y) = ((x,y),) $ case adj points (x,y) of
      [ _, _, _, _, _, _, False, False, False ] -> (x,y+1)
      [ False, _, _, False, _, _, False, _, _ ] -> (x-1,y)
      [ _, _, False, _, _, False, _, _, False ] -> (x+1,y)
      [ False, False, False, _, _, _, _, _, _ ] -> (x,y-1)
      _ -> (x,y)

    go2 :: Point -> (Point, Point)
    go2 (x,y) = ((x,y),) $ case adj points (x,y) of
      [ False, _, _, False, _, _, False, _, _ ] -> (x-1,y)
      [ _, _, False, _, _, False, _, _, False ] -> (x+1,y)
      [ False, False, False, _, _, _, _, _, _ ] -> (x,y-1)
      [ _, _, _, _, _, _, False, False, False ] -> (x,y+1)
      _ -> (x,y)

    go3 :: Point -> (Point, Point)
    go3 (x,y) = ((x,y),) $ case adj points (x,y) of
      [ _, _, False, _, _, False, _, _, False ] -> (x+1,y)
      [ False, False, False, _, _, _, _, _, _ ] -> (x,y-1)
      [ _, _, _, _, _, _, False, False, False ] -> (x,y+1)
      [ False, _, _, False, _, _, False, _, _ ] -> (x-1,y)
      _ -> (x,y)

    (should_move, should_not_move) = S.partition ((>1) . length . filter id . adj points) points

    ps' = S.map ([go0, go1, go2, go3] !! i) should_move

    check ps (p_old, p_new)
      | S.size (S.filter ((p_new ==) . snd) ps) > 1 = p_old
      | otherwise = p_new

showGrid :: Set Point -> String
showGrid rocks = unlines $ reverse rows
  where
    cell y x
      | (x,y) `S.member` rocks = '#'
      | otherwise = '.'

    min_x = S.findMin $ S.map fst rocks
    min_y = S.findMin $ S.map snd rocks
    max_x = S.findMax $ S.map fst rocks
    max_y = S.findMax $ S.map snd rocks

    row :: Int -> String
    row y = cell y <$> [min_x-3..max_x+3]

    rows = row <$> reverse [min_y-3 .. max_y+3]

firstSame :: Eq a => [a] -> [a]
firstSame (a1:a2:as) 
  | a1 == a2 = [a1]
  | otherwise = a1 : firstSame (a2:as)
firstSame _ = undefined

main :: IO ()
main = do
  input <- parse <$> readFile "input/23.txt"

  let out = firstSame $ fmap fst $ iterate go (input, 0)

  --     min_x = S.findMin $ S.map fst out
  --     min_y = S.findMin $ S.map snd out

  --     max_x = S.findMax $ S.map fst out
  --     max_y = S.findMax $ S.map snd out

  --     all_ps = S.fromList [ (x,y) | x <- [min_x..max_x], y <- [min_y..max_y] ]

  -- print [ min_x, max_x, min_y, max_y ]

  -- print $ S.size $ all_ps S.\\ out
  print $ length out