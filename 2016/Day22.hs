module Main where
import Utils (Parser, parseHardError)
import Text.Megaparsec (sepBy, some, anySingleBut, skipSome, manyTill, anySingle, (<|>), oneOf)
import Text.Megaparsec.Char (newline, hspace, char, letterChar, string)
import Text.Megaparsec.Char.Lexer (lexeme, decimal)
import Control.Monad (void, guard)
import Data.Foldable (maximumBy, find, Foldable (toList))
import Data.Function (on, (&))
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import Algorithm.Search (aStar, dijkstra)
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

manhattanDistance :: Pos -> Pos -> Int
manhattanDistance (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)

type State = (Pos, M.Map Pos Node)

nextStates :: State -> [State]
nextStates (free_position, positions) = do
  let (x,y) = free_position
  neighbor <- [ (x+1,y), (x-1,y), (x,y+1), (x,y-1) ]
  guard (neighbor `M.member` positions)

  let node = positions M.! neighbor
      positions' = positions
        & M.delete neighbor
        & M.insert free_position node

  return (neighbor, positions')

main :: IO ()
main = do
  nodes <- parseHardError parser <$> readFile "input/22.txt"

  putStr "Part 1: "
  let viable_pairs = viablePairs nodes
  print $ length viable_pairs

  let viable_nodes = fst <$> viable_pairs

      Just free_position = position <$> find ((== 0) . usedTerabyte) nodes
      grid_positions = M.fromList $ (\node -> (position node, node)) <$> viable_nodes

      initial_state :: State
      initial_state = (free_position, grid_positions)

      transition_cost _ _ = 1

      target_pos = (0,0)
      source_pos = maximumBy (compare `on` snd) $ filter ((==0) . fst)
        $ M.keys grid_positions

      cost_lower_bound :: State -> Int
      cost_lower_bound (free_position, positions) = 
        manhattanDistance (0,0) free_position + manhattanDistance free_position source_pos_current
        where
          source_pos_current = fst $ fromJust $ find ((== source_pos) . position . snd) $ M.toList positions

      is_final_state :: State -> Bool
      is_final_state (_, positions) =
        case M.lookup (0,0) positions of
          Just node -> position node == source_pos
          Nothing   -> False

  putStr "Part 2: "
  print $ aStar nextStates transition_cost cost_lower_bound is_final_state initial_state