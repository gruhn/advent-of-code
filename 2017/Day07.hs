module Main where

import Text.Megaparsec.Char (newline, lowerChar, string)
import Text.Megaparsec.Char.Lexer (decimal)
import qualified Data.Set as Set
import qualified Data.Map.Lazy as Map
import Data.Map.Lazy (Map)
import Utils (Parser, parseFile, mostCommon)
import Text.Megaparsec (sepBy)
import Control.Applicative ((<|>), Alternative (some))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import Data.Foldable (minimumBy)

type Nodes  = Map String (Int, [String])

parser :: Parser Nodes 
parser = Map.fromList <$> node `sepBy` newline
  where
    node :: Parser (String, (Int, [String]))
    node = do
      n  <- name
      w  <- weight
      cs <- children
      return (n, (w, cs))

    name :: Parser String
    name = some lowerChar

    weight :: Parser Int
    weight = string " (" *> decimal <* string ")"

    children :: Parser [String]
    children = 
      (string " -> " *> name `sepBy` string ", ") <|> pure []

findRoot :: Nodes -> String
findRoot nodes =
  let all_names = Map.keysSet nodes
      all_children = Set.fromList $ snd =<< Map.elems nodes
   in Set.findMin $ all_names Set.\\ all_children

totalWeights :: Nodes -> Map String Int
totalWeights nodes =
  let nodes' = Map.map weightOf nodes

      weightOf :: (Int, [String]) -> Int
      weightOf (weight, child_names) = 
        let child_weights = map (nodes' Map.!) child_names
         in weight + sum child_weights

   in nodes'

faultyNodes :: Nodes -> [(String, Int)]
faultyNodes nodes = do
  let total_weights = totalWeights nodes

  (_, child_names) <- Map.elems nodes

  let child_weights = map (total_weights Map.!) child_names
  let most_common = fromMaybe 0 $ mostCommon child_weights

  (name, total_weight) <- zip child_names child_weights

  guard $ total_weight /= most_common

  let original_weight = fst (nodes Map.! name)
  let corrected_weight = original_weight + (most_common - total_weight)

  return (name, corrected_weight)

isAncestorOf :: Nodes -> String -> String -> Bool
isAncestorOf nodes node1 node2 = 
  node1 == node2 || any (isAncestorOf nodes node1) (snd $ nodes Map.! node2)       

subTreeRelation :: Nodes -> String -> String -> Ordering
subTreeRelation nodes node1 node2
  | node1 == node2 = EQ
  | isAncestorOf nodes node1 node2 = LT
  | isAncestorOf nodes node2 node1 = GT
  | otherwise = EQ -- not really, since it's a partial order but doesn't matter for this purpose

main :: IO ()
main = do
  nodes <- parseFile parser "input/07.txt"

  putStr "Part 1: "
  print $ findRoot nodes

  putStr "Part 2: " 
  print 
    $ minimumBy (\(a, _) (b, _) -> subTreeRelation nodes a b) 
    $ faultyNodes nodes

