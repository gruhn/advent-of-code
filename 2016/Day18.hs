module Main where
import Data.Foldable (foldl')

-- >>> convolute 3 id 'x' "abc"
-- ["xab","abc","bcx"]

convolute :: Int -> ([a] -> b) -> a -> [a] -> [b]
convolute window_size f edge as = f <$> windows as_padded
  where
    padding = replicate (window_size `div` 2) edge
    as_padded = padding <> as <> padding

    windows :: [a] -> [[a]]
    windows as 
      | length as < window_size = [] 
      | otherwise               = take window_size as : windows (tail as)

-- >>> nextRow "..^^." == ".^^^^"
-- True

nextRow :: String -> String
nextRow = convolute 3 go '.'
  where
    -- new tile is a trap only if:
    go :: String -> Char
    -- Its left and center tiles are traps, but its right tile is not.
    go "^^." = '^'
    -- Its center and right tiles are traps, but its left tile is not.
    go ".^^" = '^'
    -- Only its left tile is a trap.
    go "^.." = '^'
    -- Only its right tile is a trap.
    go "..^" = '^'
    -- Otherwise, not a trap.
    go _     = '.'

main :: IO ()
main = do
  input <- readFile "input/18.txt"

  putStr "Part 1: "
  let rows_1 = take 40 $ iterate nextRow input
  print $ sum $ length . filter ('.' ==) <$> rows_1

  putStr "Part 2: "
  let rows_2 = take 400000 $ iterate nextRow input
  print $ sum $ length . filter ('.' ==) <$> rows_2
