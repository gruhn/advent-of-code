module Main where

import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (string, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import ParseUtils (Parser, parseWith)

type Pos = (Int, Int)

parser :: Parser [Pos]
parser = 
  let
    point :: Parser Pos
    point = do
      x <- decimal
      string ", "
      y <- decimal
      return (x, y)
  in
    point `sepEndBy` newline

dist :: Pos -> Pos -> Int
dist (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

data Size = Finite Int | Infinite

select :: [a] -> [(a,[a])]
select []     = []
select (a:as) = (a,as) : [ (a',a:as') | (a',as') <- select as ]

regionSize :: Pos -> [Pos] -> Size
regionSize pos other_poses = _

main :: IO ()
main = do 
  poses <- parseWith parser "input/06.txt"

  putStr "Part 1: "
  print $ maximum $ do 
    (pos, other_poses) <- select poses
    case regionSize pos other_poses of
      Infinite    -> []
      Finite size -> [size]
