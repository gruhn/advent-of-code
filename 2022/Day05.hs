module Main where
import Utils (Parser, parseHardError)
import Text.Megaparsec.Char (string, newline)
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char.Lexer (decimal)
import qualified Data.IntMap as M
import Data.Foldable (foldl')
import Control.Arrow (first)

{-
            [J] [Z] [G]            
            [Z] [T] [S] [P] [R]    
[R]         [Q] [V] [B] [G] [J]    
[W] [W]     [N] [L] [V] [W] [C]    
[F] [Q]     [T] [G] [C] [T] [T] [W]
[H] [D] [W] [W] [H] [T] [R] [M] [B]
[T] [G] [T] [R] [B] [P] [B] [G] [G]
[S] [S] [B] [D] [F] [L] [Z] [N] [L]
 1   2   3   4   5   6   7   8   9 
-}

type Stacks = M.IntMap String

initialStacks :: Stacks
initialStacks = M.fromList $ zip [1 ..]
  [ "RWFHTS"
  , "WQDGS"
  , "WTB"
  , "JZQNTWRD"
  , "ZTVLGHBF"
  , "GSBVCTPL"
  , "PGWTRBZ"
  , "RJCTMGN"
  , "WBGL"
  ] 

data Move = Move Int Int Int
  deriving Show

parser :: Parser [Move]
parser = move `sepBy` newline
  where
    move :: Parser Move
    move = Move 
      <$ string "move " <*> decimal 
      <* string " from " <*> decimal 
      <* string " to " <*> decimal

addCrates :: Int -> String -> Stacks -> Stacks
addCrates to crates = M.adjust (crates <>) to

removeCrates :: Int -> Int -> Stacks -> (String, Stacks)
removeCrates count from stacks = (crates, stack')
  where
    crates = take count (stacks M.! from)
    stack' = M.adjust (drop count) from stacks

topCrates :: Stacks -> String
topCrates = M.elems . fmap head

applyMove1 :: Stacks -> Move -> Stacks
applyMove1 stacks (Move count from to) =
    uncurry (addCrates to) 
  $ first reverse 
  $ removeCrates count from stacks

applyMove2 :: Stacks -> Move -> Stacks
applyMove2 stacks (Move count from to) =
    uncurry (addCrates to) 
  $ removeCrates count from stacks

main :: IO ()
main = do
  moves <- parseHardError parser <$> readFile "input/05.txt"

  putStr "Part 1: "
  print $ topCrates $ foldl' applyMove1 initialStacks moves

  putStr "Part 2: "
  print $ topCrates $ foldl' applyMove2 initialStacks moves