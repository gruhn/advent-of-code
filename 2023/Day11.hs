module Main where
import Utils (withCoords, combinations)
import qualified Data.IntSet as ISet

parse :: String -> [(Int,Int)]
parse = map fst . filter ((=='#') . snd) . withCoords . lines

dist :: (Int,Int) -> (Int,Int) -> Int
dist (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

-- | adjust coordinates for expansion
expand :: Int -> [(Int,Int)] -> [(Int,Int)]
expand factor galaxies = map go galaxies
  where
    non_empty_rows = ISet.fromList $ map snd galaxies
    non_empty_cols = ISet.fromList $ map fst galaxies

    all_rows = ISet.fromList [0 .. ISet.findMax non_empty_rows] 
    all_cols = ISet.fromList [0 .. ISet.findMax non_empty_cols] 

    empty_rows = all_rows ISet.\\ non_empty_rows
    empty_cols = all_cols ISet.\\ non_empty_cols

    go :: (Int,Int) -> (Int,Int)
    go (x,y) = (expanded_x, expanded_y)
      where
        expanded_x = x + (factor-1) * ISet.size (ISet.filter (<x) empty_cols) 
        expanded_y = y + (factor-1) * ISet.size (ISet.filter (<y) empty_rows)

main :: IO ()
main = do
  points <- parse <$> readFile "input/11.txt"

  putStr "Part 1: "
  print $ sum $ map (uncurry dist) $ combinations $ expand 2 points

  putStr "Part 2: "
  print $ sum $ map (uncurry dist) $ combinations $ expand 1000000 points
