module Main where
import Utils (Parser, parseFile, lexeme)
import Text.Megaparsec (sepBy, sepEndBy, choice, sepEndBy1)
import Text.Megaparsec.Char (newline, string, char, lowerChar)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (guard)
import Control.Applicative (some)
import Data.List (sortOn)
import Data.Semigroup (sconcat)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (listToMaybe, mapMaybe)
import Prelude hiding (lookup)
import Data.Function (on)
import Data.Foldable (minimumBy)

data Range = Range 
  { getDest :: Int
  , getSrc  :: Int
  , getLen  :: Int
  } deriving Show

newtype RangeMap = RangeMap { getRanges :: [Range] }
  deriving Show

type Rules = NonEmpty ((String, String), RangeMap)

parser :: Parser ([Int], Rules)
parser = (,) <$> seeds <*> rules
  where
    seeds :: Parser [Int]
    seeds = string "seeds: " *> decimal `sepBy` char ' ' <* string "\n\n"

    rules :: Parser Rules
    rules = NonEmpty.fromList <$> rule `sepEndBy1` newline

    rule_categories :: Parser (String, String)
    rule_categories = do
      src  <- some lowerChar
      string "-to-"
      dest <- some lowerChar
      string " map:\n"
      return (src, dest)

    rule :: Parser ((String, String), RangeMap)
    rule = do
      categories <- rule_categories
      ranges <- range `sepEndBy` newline
      return (categories, RangeMap ranges)

    range :: Parser Range
    range = Range 
      <$> lexeme decimal 
      <*> lexeme decimal 
      <*> lexeme decimal

nonEmpty :: Range -> Bool
nonEmpty range = getLen range > 0

-- add implicit ranges before, after and in between
fillRanges :: RangeMap -> RangeMap
fillRanges (RangeMap ranges) = 
  let fill :: Int -> [Range] -> [Range]
      fill index [] = [Range index index (maxBound-index)]
      fill index (range : ranges) = 
        let fill_before = Range index index (getSrc range - index)
            new_index = getSrc range + getLen range
            -- fill_before might be empty/negative range ==> filter out in second pass
         in fill_before : range : fill new_index ranges

   in RangeMap $ filter nonEmpty $ fill 0 $ sortOn getSrc ranges

instance Semigroup RangeMap where
  RangeMap rangesA <> RangeMap rangesB = RangeMap $ do
    -- compute overlap for each pair for ranges
    Range destA srcA lenA <- rangesA
    Range destB srcB lenB <- rangesB

    let dest = destB + max 0 (destA - srcB)
        src  = srcA + max 0 (srcB - destA)
        len  = min (destA+lenA) (srcB+lenB) - max destA srcB

        overlap = Range dest src len

    guard $ nonEmpty overlap
    return overlap

-- lookup destination value for a concrete source value in a RangeMap
lookup :: Int -> RangeMap -> Maybe Int
lookup num (RangeMap ranges) = listToMaybe $ do
  Range dest src len <- ranges
  guard $ src <= num && num < (src+len)
  return (num + dest - src)

rangeMapFromSeeds :: [Int] -> RangeMap
rangeMapFromSeeds = RangeMap . go
  where
    go :: [Int] -> [Range]
    go []  = []
    go [_] = error "odd number of seeds"
    go (src:len:rest) = Range src src len : go rest

main :: IO ()
main = do
  (seeds, rules) <- parseFile parser "input/05.txt"

  let squashed_range_map = sconcat $ fmap (fillRanges . snd) rules 

  putStr "Part 1: "
  print 
    $ minimum 
    $ mapMaybe (`lookup` squashed_range_map) seeds 

  putStr "Part 2: "
  print 
    $ minimum 
    $ map getDest 
    $ getRanges 
    $ rangeMapFromSeeds seeds <> squashed_range_map
