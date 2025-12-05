module Main (main) where
import Text.Megaparsec (endBy, sepEndBy)
import Text.Megaparsec.Char (char, newline)
import Utils (Parser, parseFile)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.List (sort)
import Control.Monad (guard)

type Range = (Int, Int)

parser :: Parser ([Range], [Int])
parser = do
  let range = (,) <$> decimal <* char '-' <*> decimal
  ranges <- range `endBy` newline
  newline
  ingredients <- decimal `sepEndBy` newline
  return (ranges, ingredients)

inRange :: Int -> Range -> Bool
inRange x (a,b) = a <= x && x <= b

isFresh :: [Range] -> Int -> Bool
isFresh ranges ingredient = any (inRange ingredient) ranges

tryMergeRanges :: Range -> Range -> Maybe Range
tryMergeRanges (a1,b1) (a2,b2) = do
  guard $ a1 <= b2+1 && a2 <= b1+1
  return (min a1 a2, max b1 b2)

deduplicate :: [Range] -> [Range]
deduplicate = go . sort
  where
    go :: [Range] -> [Range]
    go [] = []
    go [range] = [range]
    go (range1:range2:rest) =
      case tryMergeRanges range1 range2 of
        Nothing -> range1 : go (range2:rest)
        Just merged -> go (merged:rest)

rangeSize :: Range -> Int
rangeSize (a,b) = b - a + 1

main :: IO ()
main = do
  (ranges, ingredients) <- parseFile parser "input/05.txt"
  let ranges_dedup = deduplicate ranges

  putStr "Part 1: "
  print $ length $ filter (isFresh ranges_dedup) ingredients

  putStr "Part 2: "
  print $ sum $ map rangeSize ranges_dedup

