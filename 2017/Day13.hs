module Main where
import Utils (Parser, parseFile)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (string, newline)
import Text.Megaparsec (sepBy)
import Control.Monad (guard)

parser :: Parser [(Int,Int)]
parser = line `sepBy` newline
 where
  line = (,) <$> decimal <* string ": " <*> decimal

isCaught :: Int -> (Int,Int) -> Bool
isCaught delay (depth,range) = 
 (depth+delay) `mod` (2*range-2) == 0

main :: IO ()
main = do
 layers <- parseFile parser "input/13.txt"

 putStr "Part 1: "
 print $ sum $ do 
  (depth,range) <- layers
  guard $ isCaught 0 (depth,range)
  return $ depth*range

 putStr "Part 2: "
 print $ head $ do 
  delay <- [0..]
  guard $ not $ any (isCaught delay) layers
  return delay
