module Main where

import Utils (Parser, parseFile)
import Text.Megaparsec.Char (lowerChar, char, string, newline)
import Text.Megaparsec (sepBy, sepEndBy)
import Control.Applicative (some)
import Graph (Edge(Edge), Node, NodeSet)
import qualified Graph
import Data.Containers.ListUtils (nubOrd)
import Data.Map.Strict (Map)
import qualified Data.Map as Map
import qualified Data.IntSet as IntSet
import Control.Monad (guard)

type Graph = Graph.Graph ()

parser :: Parser [(String, [String])]
parser = 
  let 
    node_label :: Parser String
    node_label = some lowerChar

    line :: Parser (String, [String])
    line = do 
      source  <- node_label
      string ": "
      targets <- node_label `sepBy` char ' '
      return (source, targets)
  in
    line `sepEndBy` newline

mkGraph :: [(String, [String])] -> Graph
mkGraph connections = 
  let
    labels :: [String]
    labels = nubOrd $ do
      (source, targets) <- connections
      source : targets

    labels_to_nodes :: Map String Node
    labels_to_nodes = Map.fromList $ zip labels [1..]

    edges :: [Edge ()]
    edges = do
      (source, targets) <- connections
      let source_node = labels_to_nodes Map.! source
      target <- targets
      let target_node = labels_to_nodes Map.! target
      [ Edge source_node target_node (), Edge target_node source_node () ]
  in 
    Graph.fromEdges edges

main :: IO ()
main = do
  graph <- mkGraph <$> parseFile parser "input/25.txt"

  let total_node_count = IntSet.size $ Graph.nodeSet graph

  putStr "Part 1: "
  print $ do
    component <- Graph.stoerWagner graph
    guard $ Graph.componentOutDegree component graph == 3
    let component_size = IntSet.size component
    return $ (total_node_count - component_size) * component_size
