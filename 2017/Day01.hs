module Main where

import Utils (Parser, parseFile)
import Text.Megaparsec.Char (digitChar)
import Text.Megaparsec (many)
import Data.Char (digitToInt)
import Data.List (group, stripPrefix)
import qualified Data.Vector as V

parser :: Parser [Int]
parser = many (digitToInt <$> digitChar)

main :: IO ()
main = do 
  digits <- V.fromList <$> parseFile parser "input/01.txt"

  let score :: (Int -> Int) -> Int
      score partner_of = sum (V.ifilter is_match digits)
        where
          is_match index digit = digits V.! partner_of index == digit

  putStr "Part 1: "
  print $ score $ \index -> (index + 1) `mod` length digits

  putStr "Part 2: "
  print $ score $ \index -> (index + length digits `div` 2) `mod` length digits
