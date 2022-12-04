module Main where
import Utils
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec (sepBy)

type Range = (Int, Int)

parser :: Parser [(Range, Range)]
parser = pair `sepBy` newline
  where
    range :: Parser Range
    range = (,) <$> decimal <* char '-' <*> decimal

    pair :: Parser (Range, Range)
    pair = (,) <$> range <* char ',' <*> range

contained :: Range -> Range -> Bool
contained (l1, u1) (l2, u2)
  | l1 <= l2  = u2 <= u1
  | otherwise = u1 <= u2

overlap :: Range -> Range -> Bool
overlap (l1, u1) (l2, u2)
  | l1 <= l2  = l2 <= u1
  | otherwise = l1 <= u2

main :: IO ()
main = do
  pairs <- parseHardError parser <$> readFile "input/04.txt"

  putStr "Part 1: "
  print $ length $ filter (uncurry contained) pairs

  putStr "Part 2: "
  print $ length $ filter (uncurry overlap) pairs