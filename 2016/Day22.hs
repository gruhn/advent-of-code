module Main where
import Utils (Parser, parseHardError)
import Text.Megaparsec (sepBy, some, anySingleBut, skipSome, manyTill, anySingle, (<|>), oneOf)
import Text.Megaparsec.Char (newline, hspace, char, letterChar, string)
import Text.Megaparsec.Char.Lexer (lexeme, decimal)
import Control.Monad (void, guard)
import Data.Foldable (maximumBy, find, Foldable (toList), for_)
import Data.Function (on, (&))
import qualified Data.Set as S
import Algorithm.Search (dijkstra)
import Data.Maybe (fromJust)

type Pos = (Int, Int)

data Node = Node
  { position :: Pos
  , sizeTerabyte :: Int
  , usedTerabyte :: Int
  , availTerabyte :: Int
  , usePercent :: Int
  } deriving (Ord, Eq, Show)

parser :: Parser [Node]
parser = line *> node `sepBy` newline
  where
    line :: Parser String
    line = anySingle `manyTill` newline

    lex :: Parser a -> Parser a
    lex = lexeme (void hspace)

    position :: Parser Pos
    position = lex $ do
      string "/dev/grid/node-x"
      x <- decimal
      string "-y"
      y <- decimal
      return (x,y)

    terabyte :: Parser Int
    terabyte = lex (decimal <* char 'T')

    percent :: Parser Int
    percent = lex (decimal <* char '%')

    node :: Parser Node
    node = Node
      <$> position
      <*> terabyte
      <*> terabyte
      <*> terabyte
      <*> percent

viablePairs :: [Node] -> [(Node, Node)]
viablePairs nodes = do
  a <- nodes
  guard (usedTerabyte a > 0)
  b <- nodes
  guard (a /= b)
  guard (usedTerabyte a <= availTerabyte b)
  return (a,b)

type Swap = (Pos,Pos)

findSwapPath :: S.Set Pos -> Pos -> Pos -> Pos -> Maybe [Swap]
findSwapPath viable_positions source target free = direct_path >>= go source free
  where
    neighbors :: S.Set Pos -> Pos -> [Pos]
    neighbors viable_positions (x,y) = do
      pos <- [ (x+1,y), (x-1,y), (x,y+1), (x,y-1) ]
      guard (pos `S.member` viable_positions)
      return pos

    step_cost _ _ = 1
  
    direct_path = snd <$> dijkstra (neighbors viable_positions) step_cost (==target) source

    go :: Pos -> Pos -> [Pos] -> Maybe [Swap]
    go current free_pos [] = Just []
    go current free_pos (next:path) = do 
      -- force path to go around `current`
      let viable = S.delete current viable_positions
      -- bring `free_node` in front of `data_node`
      free_path <- snd <$> dijkstra (neighbors viable) step_cost (==next) free_pos
      let free_path_swaps = zip free_path (free_pos:free_path) 
      -- swap `data_node` for `next` (where `free_node` is now)
      let data_swap = (current, next)
      -- continue with `free_pos` as the old `current` and the new `current` is `next`
      rest_swaps <- go next current path
      return $ free_path_swaps <> [data_swap] <> rest_swaps

main :: IO ()
main = do
  nodes <- parseHardError parser <$> readFile "input/22.txt"

  putStr "Part 1: "
  let viable_pairs = viablePairs nodes
  print $ length viable_pairs

  putStr "Part 2: "
  let viable_nodes = fst <$> viable_pairs
      Just free_node = find ((== 0) . usedTerabyte) nodes

      viable_poses = S.fromList $ position <$> (free_node : viable_nodes)
      free_pos = position free_node
      target_pos = (0,0)
      source_pos = maximumBy (compare `on` fst) $ S.filter ((==0) . snd) viable_poses

  print $ length <$> findSwapPath viable_poses source_pos target_pos free_pos