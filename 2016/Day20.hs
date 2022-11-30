module Main where

import qualified Data.List as L
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (newline, char)
import Text.Megaparsec.Char.Lexer (decimal)
import Utils (Parser, parseHardError)
import Data.Foldable (for_)

newtype Range = Range { getRange :: [(Int,Int)] }
  deriving Show

singleton :: Int -> Int -> Range
singleton x y = Range [(min x y, max x y)]

reduce :: Range -> Range
reduce = Range . union . L.sortOn fst . getRange
  where
    union :: [(Int,Int)] -> [(Int,Int)]
    union []  = []
    union [r] = [r]
    union ((low1,high1):(low2,high2):rs)
      | high1+1 < low2 = (low1,high1) : union ((low2,high2):rs)
      | otherwise      = union ((low1,max high1 high2):rs)

instance Semigroup Range where
  Range rs1 <> Range rs2 = reduce (Range (rs1 <> rs2))

instance Monoid Range where
  mempty = Range []

exampleRange :: Range
exampleRange = Range $ [(1,3),(10,20)]

-- >>> values exampleRange
-- [1,2,3,10,11,12,13,14,15,16,17,18,19,20]

values :: Range -> [Int]
values (Range rs) = rs >>= \(low,high) -> [low..high]

-- >>> invert exampleRange
-- Range {getRange = [(0,0),(4,9),(21,4294967295)]}

invert :: Range -> Range
invert (Range rs) = Range $ bottom <> between <> top
  where
    min_bound = 0 
    max_bound = 2^32-1

    (lowest, _)  = head rs
    (_, highest) = last rs

    bottom 
      | min_bound < lowest = [(min_bound, lowest-1)]
      | otherwise  = []

    top 
      | highest < max_bound = [(highest+1, max_bound)]
      | otherwise           = []

    between = 
      let middle (_,high1) (low2,_) = (high1+1, low2-1) 
      in  zipWith middle rs (tail rs)

-- >>> size exampleRange
-- 14

size :: Range -> Int
size (Range rs) = sum (go <$> rs)
  where
    go :: (Int,Int) -> Int
    go (low, high) = high - low + 1

parser :: Parser [Range]
parser = range `sepBy` newline
  where
    range :: Parser Range
    range = singleton
      <$> decimal 
      <* char '-' 
      <*> decimal

main :: IO ()
main = do
  ranges <- parseHardError parser <$> readFile "input/20.txt"

  let blocked_ips = mconcat ranges
      allowed_ips = invert blocked_ips

  putStr "Part 1: "
  print $ head $ values allowed_ips

  putStr "Part 2: "
  print $ size allowed_ips
