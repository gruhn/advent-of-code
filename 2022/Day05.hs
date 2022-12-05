module Main where
import Utils (Parser, parseHardError)
import Text.Megaparsec.Char (string, newline)
import Text.Megaparsec (sepBy, anySingleBut, sepEndBy)
import Text.Megaparsec.Char.Lexer (decimal)
import qualified Data.IntMap as M
import Data.Foldable (foldl')
import Control.Arrow (first)
import Control.Applicative (some)
import Data.List ( transpose )

type Stacks = M.IntMap String
data Move = Move Int Int Int
  deriving Show

parser :: Parser (Stacks, [Move])
parser = (,) <$> stacks <* newline <*> moves
  where
    move :: Parser Move
    move = Move
      <$ string "move " <*> decimal
      <* string " from " <*> decimal
      <* string " to " <*> decimal

    moves :: Parser [Move]
    moves = move `sepBy` newline

    stacks_section :: Parser [String]
    stacks_section = some (anySingleBut '\n') `sepEndBy` newline

    drop_filler :: [String] -> [String]
    drop_filler (_:a:_:_:as) = a : drop_filler as
    drop_filler (_:a:_) = [a]
    drop_filler _ = []

    trim_left :: String -> String
    trim_left = dropWhile (==' ')

    stacks :: Parser Stacks
    stacks = do
      lines <- init <$> stacks_section
      let columns = trim_left <$> drop_filler (transpose lines)
      return $ M.fromList $ zip [1..] columns

addCrates :: Int -> String -> Stacks -> Stacks
addCrates to crates = M.adjust (crates <>) to

removeCrates :: Int -> Int -> Stacks -> (String, Stacks)
removeCrates count from stacks = (crates, stacks')
  where
    crates = take count (stacks M.! from)
    stacks' = M.adjust (drop count) from stacks

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
  (stacks, moves) <- parseHardError parser <$> readFile "input/05.txt"

  putStr "Part 1: "
  print $ topCrates $ foldl' applyMove1 stacks moves

  putStr "Part 2: "
  print $ topCrates $ foldl' applyMove2 stacks moves