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
    distance :: Pos -> Pos -> Float
    distance (x,y) (x',y') = 
        (fromIntegral x - fromIntegral x')^2 
      + (fromIntegral y - fromIntegral y')^2

moveTail :: Pos -> Pos -> Pos
moveTail successor_pos pos
  | pos `elem` neighborhoodOf successor_pos = pos
  | otherwise = valid_positions `closestTo` successor_pos 
    where
      valid_positions = neighborhoodOf successor_pos `S.intersection` neighborhoodOf pos

step :: (Pos, [Pos]) -> Move -> (Pos, [Pos])
step (head_pos, tail_poses) move = (new_head_pos, new_tail_poses)
  where
    new_head_pos = moveHead head_pos move
    new_tail_poses = tail $ scanl' moveTail new_head_pos tail_poses

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