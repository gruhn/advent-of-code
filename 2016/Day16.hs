module Main where
import qualified Data.List as L

main :: IO ()
main = do
  let input = "11100010111110100" 
      fill_length = 272
      -- fill_length = 35651584

  putStr "Part 1: "
  print $ length $ checksum $ generateData fill_length input

  putStr "Part 2: "

-- >>> "111000101111101001"
-- >>> invertReverse "111000101111101001"
-- >>> checksum "111000101111101001" 
-- >>> reverse $ checksum (invertReverse "111000101111101001")
-- "111000101111101001"
-- "011010000010111000"
-- "101011000"
-- "101011000"

invertReverse :: String -> String
invertReverse str = L.reverse (invert <$> str)
  where
    invert '1' = '0'
    invert '0' = '1'
    invert _   = undefined

generateData :: Int -> String -> String
generateData fill_length = 
  take fill_length . head . dropWhile not_enough_data . iterate step
  where
    not_enough_data str = length str < fill_length
    step str = str <> "0" <> invertReverse str


chunks :: Int -> [a] -> [[a]]
chunks len list
  | length list < len = []
  | otherwise = chunk : chunks len rest
  where
    (chunk, rest) = L.splitAt len list

checksum :: String -> String
checksum = head . dropWhile (even . length) . iterate step
  where
    combine "00" = "1"
    combine "11" = "1"
    combine "10" = "0"
    combine "01" = "0"
    combine _    = undefined

    step = concatMap combine . chunks 2
