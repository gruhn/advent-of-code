{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where
import Utils (Parser, parseFile, count, chunksOf)
import Text.Megaparsec (sepBy, oneOf, some, sepEndBy)
import Text.Megaparsec.Char (char, string, newline)
import Data.List (transpose)
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Map as Map

newtype Grid a = Grid { getRows :: [[a]] }
  deriving (Eq, Ord, Show)

transform :: ([[a]] -> [[b]]) -> Grid a -> Grid b
transform f = Grid . f . getRows

size :: Grid a -> Int
size (Grid rows) = length rows

instance Functor Grid where
  fmap f = transform $ map (map f)

instance Semigroup (Grid a) where
  Grid rows1 <> Grid rows2 = Grid $ zipWith (<>) rows1 rows2

divide :: Grid a -> Grid (Grid a)
divide grid = 
  let len = if even (size grid) then 2 else 3 in
  transform (transpose . map (map Grid . chunksOf len) . transpose . map (chunksOf len)) grid

merge :: Grid (Grid a) -> Grid a
merge = transform $ concatMap (getRows . foldr1 (<>))

variants :: Ord a => Grid a -> [Grid a]
variants = nubOrd . take 8 . go_flip
  where
    go_flip :: Grid a -> [Grid a]
    go_flip grid = grid : go_transpose (transform (map reverse) grid)

    go_transpose :: Grid a -> [Grid a]
    go_transpose grid = grid : go_flip (transform transpose grid)

type Rule = (Grid Char, Grid Char)

parser :: Parser [Rule]
parser = rule `sepEndBy` newline
  where
    rule :: Parser Rule
    rule = (,) <$> pattern <* string " => " <*> pattern

    pattern_row :: Parser String
    pattern_row = some $ oneOf ("#." :: String)

    pattern :: Parser (Grid Char)
    pattern = Grid <$> pattern_row `sepBy` char '/'

main :: IO ()
main = do
  rules <- parseFile parser "input/21.txt"

  let rules_with_variants = do
        (source, target) <- rules
        source_variant <- variants source
        return (source_variant, target)

      rule_map = Map.fromListWith (error "duplicate pattern") rules_with_variants

      enhance :: Grid Char -> Grid Char
      enhance = merge . fmap (rule_map Map.!) . divide

      count_on :: Grid Char -> Int
      count_on = sum . map (count (== '#')) . getRows
  
      start_grid = Grid [".#.","..#","###"]

  putStr "Part 1: "
  print $ count_on $ iterate enhance start_grid !! 5

  putStr "Part 2: "
  print $ count_on $ iterate enhance start_grid !! 18

