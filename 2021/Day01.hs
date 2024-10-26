module Main (main) where 

import Text.Parsec.String (Parser)
import Text.Parsec.Char (newline, char)
import Text.Parsec (sepEndBy, eof, parse, choice, many1)
import Text.Parsec.Char (digit)
import Data.List (scanl')
import qualified Data.IntSet as Set
import Data.IntSet (IntSet)

parser :: Parser [Int]
parser = integer `sepEndBy` newline
  where
    decimal :: Parser Int
    decimal = read <$> many1 digit

    integer :: Parser Int
    integer = choice 
      [           char '+'  *> decimal
      , negate <$ char '-' <*> decimal 
      ]

parseOrFail :: String -> [Int]
parseOrFail input = 
  case parse (parser <* eof) "" input of
    Left  err -> error $ show err
    Right res -> res

duplicates :: [Int] -> [Int]
duplicates = go Set.empty
  where
    go :: IntSet -> [Int] -> [Int]
    go _ [] = []
    go seen (x:xs) 
      | Set.member x seen = x : go seen xs
      | otherwise = go (Set.insert x seen) xs
  
main :: IO ()
main = do
  nums <- parseOrFail <$> readFile "input/01.txt"

  putStr "Part 1: "
  print $ sum nums

  putStr "Part 2: "
  print $ head $ duplicates $ scanl' (+) 0 $ cycle nums
