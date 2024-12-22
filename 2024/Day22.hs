module Main (main) where
import Utils (Parser, integer, parseFile)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec (sepEndBy)
import Data.Bits (xor)
import Data.List (tails)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

parser :: Parser [Int]
parser = integer `sepEndBy` newline

next :: Int -> Int
next num0 =
  let
    num1 = prune $ num0 `xor` (num0 * 64)
    num2 = prune $ num1 `xor` (num1 `div` 32)
    num3 = prune $ num2 `xor` (num2 * 2048)
  in
    num3

prune :: Int -> Int
prune num = num `mod` 16777216

prices :: Int -> [Int]
prices = map (`mod` 10) . iterate next

type PriceMap = IntMap Int

{-| 
  Encode list of integers in the range [-9, 9] as a single integer
  for more efficient keys in the PriceMap.
-}
encode :: [Int] -> Int
encode []     = 0
encode (x:xs) = (x+9) + 19*encode xs

buildPriceMap :: [Int] -> PriceMap
buildPriceMap price_changes = IntMap.fromListWith (\_ old -> old) $ do
  p1:p2:p3:p4:p5:_ <- tails price_changes
  let diffs = [p2-p1, p3-p2, p4-p3, p5-p4]
  return (encode diffs, p5)

main :: IO ()
main = do
  input <- parseFile parser "input/22.txt"

  putStr "Part 1: "
  print $ sum [ iterate next seed !! 2000 | seed <- input ]

  putStr "Part 2: "
  let price_maps = [ buildPriceMap $ take 2001 $ prices seed | seed <- input ]
  print $ maximum $ IntMap.elems $ IntMap.unionsWith (+) price_maps
