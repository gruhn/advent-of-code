{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Fuse foldr/map" #-}
{-# HLINT ignore "Use map once" #-}
module Main where
import Utils (Parser, parseFile)
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (upperChar, newline, char, string)
import Control.Applicative (many, (<|>), some)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Foldable (traverse_)
import Control.Monad (guard)
import Data.List (isSuffixOf, findIndex, elemIndex)
import Data.Maybe (fromJust)

data Instr = Lft | Rgt
  deriving Show

type Graph = Map String (String, String)

parser :: Parser ([Instr], Graph)
parser = (,) <$> instructions <*> graph
  where
    left :: Parser Instr
    left = Lft <$ char 'L'

    right :: Parser Instr
    right = Rgt <$ char 'R'

    instructions :: Parser [Instr]
    instructions = many (left <|> right) <* string "\n\n"

    graph :: Parser Graph
    graph = Map.fromList <$> node `sepEndBy` newline

    node :: Parser (String, (String, String))
    node = do
      from <- some upperChar
      string " = ("
      left <- some upperChar
      string ", "
      right <- some upperChar
      string ")"
      return (from, (left, right))

step :: Graph -> (String, [Instr]) -> (String, [Instr])
step graph (node, instrs) =
  case (instrs, Map.lookup node graph) of
    (_, Nothing) -> error "node not contained in graph"
    ([], _)      -> error "instruction list is finite"
    (Lft : rem_instrs, Just (left, _)) -> (left, rem_instrs)
    (Rgt : rem_instrs, Just (_, right)) -> (right, rem_instrs)

path :: Graph -> [Instr] -> String ->  [String]
path graph instrs start = map fst $ iterate (step graph) (start, cycle instrs)

isStartNode :: String -> Bool
isStartNode node = "A" `isSuffixOf` node

isEndNode :: String -> Bool
isEndNode node = "Z" `isSuffixOf` node

main :: IO ()
main = do
  (instrs, graph) <- parseFile parser "input/08.txt"

  putStr "Part 1: "
  print $ elemIndex "ZZZ" $ path graph instrs "AAA"

  putStr "Part 2: "
  print
    -- least common multiple of all cycle lengths:
    $ foldr lcm 1
    -- findIndex never returns Nothing since paths are infinite:
    $ map fromJust
    -- first index of end node == cycle length:
    $ map (findIndex isEndNode . path graph instrs)
    -- get all start nodes:
    $ filter isStartNode $ Map.keys graph
