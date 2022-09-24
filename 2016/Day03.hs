module Main where
import Utils (Parser, parseHardError)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (hspace, newline, char)
import Text.Megaparsec (sepBy, errorBundlePretty, parse, sepBy1, count)
import Data.List (transpose)

type Triangle = [Int]

parser :: Parser [Triangle]
parser = triangle `sepBy` newline
  where
    side_length = hspace *> decimal
    
    triangle :: Parser Triangle
    triangle = count 3 side_length

validTriangle :: Triangle -> Bool
validTriangle [a,b,c] = 
     a < b + c 
  && b < a + c 
  && c < a + b
validTriangle _ = False

-- >>> chunksOf 3 [1,2,3,1,2,3,1,2,3,4]
-- [[1,2,3],[1,2,3],[1,2,3],[4]]

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n as = take n as : chunksOf n (drop n as)

main :: IO ()
main = do
  triangles <- parseHardError parser <$> readFile "2016/input/03.txt"

  putStr "Part 1: "
  print 
    $ length . filter validTriangle 
    $ triangles

  putStr "Part 2: "
  print 
    $ length . filter validTriangle
    $ chunksOf 3 . concat . transpose 
    $ triangles