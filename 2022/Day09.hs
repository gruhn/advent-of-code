{-# LANGUAGE TupleSections #-}
module Main where
import Utils
import Text.Megaparsec (sepBy, choice)
import Text.Megaparsec.Char (newline, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Foldable (minimumBy, for_)
import Data.Function (on)
import Data.List (scanl')
import qualified Data.Set as S

data Move = U | D | L | R
  deriving Show

expand :: [(Move, Int)] -> [Move]
expand moves = do
  (move, count) <- moves
  replicate count move

parser :: Parser [Move]
parser = expand <$> move `sepBy` newline
  where
    move = choice [up, down, left, right]
    up    = (U,) <$ string "U " <*> decimal
    down  = (D,) <$ string "D " <*> decimal
    left  = (L,) <$ string "L " <*> decimal
    right = (R,) <$ string "R " <*> decimal

type Pos = (Int, Int)

moveHead :: Pos -> Move -> Pos
moveHead (x,y) move =
  case move of
    U -> (x, y-1); D -> (x, y+1);
    L -> (x-1, y); R -> (x+1, y)

neighborhoodOf :: Pos -> S.Set Pos
neighborhoodOf (x,y) = S.fromList
  [ (x+dx,y+dy) | dx <-[-1..1], dy <- [-1..1] ]

closestTo :: S.Set Pos -> Pos -> Pos
closestTo options target = minimumBy (compare `on` distance target) options
  where
    distance :: Pos -> Pos -> Int
    distance (x1,y1) (x2,y2) = (x1-x2)^2 + (y1-y2)^2

follow :: Pos -> Pos -> Pos
follow pos successor 
  | pos `elem` neighborhoodOf successor = pos
  | otherwise = valid_positions `closestTo` successor
    where
      valid_positions = neighborhoodOf successor `S.intersection` neighborhoodOf pos

step :: (Pos, [Pos]) -> Move -> (Pos, [Pos])
step (head_pos, tail_poses) move = (new_head_pos, new_tail_poses)
  where
    new_head_pos = moveHead head_pos move
    new_tail_poses = tail $ scanl' (flip follow) new_head_pos tail_poses

main :: IO ()
main = do
  moves <- parseHardError parser <$> readFile "input/09.txt"

  let start_pos = (0,0)
      tail_visit_count = length . S.fromList . fmap (last . snd)

  putStr "Part 1: "
  print 
    $ tail_visit_count
    $ scanl' step (start_pos, [start_pos]) moves

  putStr "Part 1: "
  print 
    $ tail_visit_count
    $ scanl' step (start_pos, replicate 9 start_pos) moves