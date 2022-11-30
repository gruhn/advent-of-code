{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import Utils (Parser, parseHardError)
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Control.Applicative ((<|>))
import Text.Megaparsec (sepBy)
import Data.Foldable (foldl')
import Data.List (scanl')
import Data.Set (Set)
import qualified Data.Set as S

data Turn = L | R
  deriving Show

type Move = (Turn, Int)

parser :: Parser [Move]
parser = move `sepBy` string ", "
  where
    left  = do 
      char 'L' 
      magnitude <- decimal
      return (L, magnitude)

    right = do 
      char 'R' 
      magnitude <- decimal
      return (R, magnitude)

    move = left <|> right

data Direc = N | E | S | W
  deriving Show

type Pos = (Int,Int)

turn :: Turn -> Direc -> Direc
turn turn_to direc =
  case (turn_to, direc) of
    (L, N) -> W; (R, N) -> E;
    (L, E) -> N; (R, E) -> S;
    (L, S) -> E; (R, S) -> W;
    (L, W) -> S; (R, W) -> N;

seek :: Int -> (Direc, Pos) -> (Direc, Pos)
seek n (N, (x, y)) = (N, (x  , y-n))
seek n (E, (x, y)) = (E, (x+n, y  ))
seek n (S, (x, y)) = (S, (x  , y+n))
seek n (W, (x, y)) = (W, (x-n, y  ))

manhattanDistance :: Pos -> Int
manhattanDistance (x,y) = abs x + abs y

walk :: (Direc, Pos) -> [Move] -> [(Direc, Pos)]
walk (direc, pos) [] = []
walk (direc, pos) ((turn_to, mag) : moves) = tail steps ++ walk (new_direc, new_pos) moves
  where
    new_direc = turn turn_to direc
    steps     = take (mag+1) $ iterate (seek 1) (new_direc, pos)
    new_pos   = snd $ last steps

-- >>> duplicates [1,1,2,4,1,3,2,2,3]
-- [1,1,2,2,3]

-- prop> \(list :: String) -> Data.List.sort (duplicates list ++ Data.List.nub list) == Data.List.sort list
-- +++ OK, passed 100 tests.

duplicates :: forall a. Ord a => [a] -> [a]
duplicates = go mempty
  where
    go :: Set a -> [a] -> [a]
    go seen [] = []
    go seen (a:as)
      | a `S.member` seen = a : go seen as
      | otherwise         = go (S.insert a seen) as

main :: IO ()
main = do
  moves <- parseHardError parser <$> readFile "input/01.txt"

  let path = walk (N,(0,0)) moves

  putStr "Part 1: "
  print $ manhattanDistance . snd . last $ path

  putStr "Part 2: "
  print $ manhattanDistance . head . duplicates $ fmap snd path