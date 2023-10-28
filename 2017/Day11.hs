module Main where
import Utils (Parser, parseFile)
import Text.Megaparsec (sepBy, choice)
import Text.Megaparsec.Char (char, string)

type Vec = [Int]

parser :: Parser [Vec]
parser = dir `sepBy` char ','
 where
  -- https://www.redblobgames.com/grids/hexagons/
  dir :: Parser Vec
  dir = choice 
   [ [ 1, 0,-1] <$ string "ne"
   , [ 0, 1, 1] <$ string "nw"
   , [ 1, 1, 0] <$ string "n"
   , [ 0,-1,-1] <$ string "se"
   , [-1, 0, 1] <$ string "sw"
   , [-1,-1, 0] <$ string "s"
   ]

dist :: Vec -> Int
dist vec = sum (map abs vec) `div` 2

main :: IO ()
main = do
 steps <- parseFile parser "input/11.txt"

 let dist_per_step = dist <$> scanl (zipWith (+)) [0,0,0] steps

 putStr "Part 1: "
 print $ last dist_per_step 

 putStr "Part 2: "
 print $ maximum dist_per_step 

