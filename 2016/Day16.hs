module Main where
import qualified Data.List as L

interleave :: [a] -> [a] -> [a]
interleave []     bs = bs
interleave (a:as) bs = a : head bs : interleave as (tail bs)

generateData :: String -> String
generateData str = go separators
  where
    invert '1' = '0'
    invert '0' = '1'
    invert _   = undefined

    str_inv_rev = L.reverse (invert <$> str)

    separators :: String
    separators = cycle "01" `interleave` separators

    go :: String -> String
    go (sep1:sep2:seps) = str <> (sep1 : str_inv_rev) <> (sep2 : go seps)
    go _ = error "separators should be infinite"

checksum :: String -> String
checksum str = combine chunk <> checksum rest
  where
    (chunk, rest) = L.splitAt 2 str

    combine "00" = "1"
    combine "11" = "1"
    combine "10" = "0"
    combine "01" = "0"
    combine _    = undefined

-- >>> factorOutTwos 272
-- >>> factorOutTwos 35651584
-- (4,17)
-- (21,17)

factorOutTwos :: Int -> (Int, Int)
factorOutTwos x 
  | odd x = (0, x)
  | otherwise = (two_count + 1, rem_factors)
  where
    (two_count, rem_factors) = factorOutTwos (x `div` 2)

checksumWithFillLength :: Int -> String -> String
checksumWithFillLength fill_length = take output_length . repeated_checksum
  where
    (checksum_apply_count, output_length) = factorOutTwos fill_length
    repeated_checksum = (!! checksum_apply_count) . iterate checksum

main :: IO ()
main = do
  let input = "11100010111110100" 
      data_stream = generateData input

  putStr "Part 1: "
  print $ checksumWithFillLength 272 data_stream

  putStr "Part 2: "
  print $ checksumWithFillLength 35651584 data_stream