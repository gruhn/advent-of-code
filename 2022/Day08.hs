module Main where
import Data.Char (digitToInt)
import Utils (takeUntil, zipWith2D, count)
import Data.List ( transpose, mapAccumL )

type Grid = [[Int]]

map2D :: (a -> b) -> [[a]] -> [[b]]
map2D f = fmap (fmap f)

walkGrid :: ([Int] -> [Int]) -> Grid -> [[Int]]
walkGrid f rows = concat combined_grid
  where
    cols = transpose rows

    from_west = f <$> rows
    from_east = reverse . f . reverse <$> rows
    from_north = transpose (f <$> cols)
    from_south = transpose (reverse . f . reverse <$> cols)

    views = [from_west, from_east, from_north, from_south]

    empty_combined_grid = map2D (const []) rows

    combined_grid = foldr (zipWith2D (:)) empty_combined_grid views

subtractMax :: [Int] -> [Int]
subtractMax = snd . mapAccumL (\m a -> (max m a, a-m)) 0

viewDistance :: [Int] -> [Int]
viewDistance [] = []
viewDistance (a:as) =
  length (takeUntil (a <=) as) : viewDistance as

parse :: String -> Grid
parse = fmap (fmap ((+1) . digitToInt)) . lines

main :: IO ()
main = do
  grid <- parse <$> readFile "input/08.txt"

  putStr "Part 1: "
  print $ count (> 0) $ maximum <$> walkGrid subtractMax grid

  putStr "Part 2: "
  print $ maximum $ product <$> walkGrid viewDistance grid