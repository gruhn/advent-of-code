module Main where

import Utils (Parser, parseFile, safeMinimum)
import Text.Megaparsec.Char (newline, char)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec (sepEndBy)
import qualified Data.List as List
import Control.Monad (guard)
import Graph (Graph, Node, Edge(..))
import qualified Graph
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

data Pos = Pos { getX :: Int, getY :: Int, getZ :: Int }
  deriving (Show, Eq)

data Brick = Brick Pos Pos
  deriving (Eq, Show)

elevation :: Brick -> Int
elevation (Brick bottom _) = getZ bottom

parser :: Parser [Brick]
parser =
  let
    pos :: Parser Pos
    pos = Pos 
      <$> decimal <* char ','
      <*> decimal <* char ','
      <*> decimal

    brick :: Parser Brick
    brick = Brick <$> pos <* char '~' <*> pos
  in
    brick `sepEndBy` newline

haveOverlap :: Brick -> Brick -> Bool
haveOverlap (Brick start1 end1) (Brick start2 end2) =
  let
    have_x_overlap :: Bool
    have_x_overlap =
      max (getX start1) (getX start2) <= min (getX end1) (getX end2)

    have_y_overlap :: Bool
    have_y_overlap =
      max (getY start1) (getY start2) <= min (getY end1) (getY end2)
  in
    have_x_overlap && have_y_overlap

type SupportGraph = Graph Int

zDist :: Brick -> Brick -> Int
zDist (Brick _ top) (Brick bottom _) = getZ bottom - getZ top - 1

mkSupportGraph :: [Brick] -> SupportGraph
mkSupportGraph bricks = 
  let
    -- | Virtual brick that representing the ground. Without this, the
    --   "fall algorithm" does not process the lowest bricks.
    ground :: Brick 
    ground = 
      let
        max_x = maximum [ getX top | Brick _ top <- bricks ]
        min_x = minimum [ getX bot | Brick bot _ <- bricks ]
        max_y = maximum [ getY top | Brick _ top <- bricks ]
        min_y = minimum [ getY bot | Brick bot _ <- bricks ]
      in
        Brick (Pos min_x min_y 0) (Pos max_x max_y 0)

    all_nodes :: [(Node, Brick)]
    all_nodes = zip [0..] $ ground : List.sortOn elevation bricks

    edges_for :: (Node, Brick) -> [(Node, Brick)] -> [Edge Int]
    edges_for (node_from, brick_below) bricks_above = do
      (node_to, brick_above) <- bricks_above
      guard (haveOverlap brick_below brick_above)
      return $ Edge node_from node_to (zDist brick_below brick_above)

    collect_edges :: [(Node, Brick)] -> [Edge Int]
    collect_edges [] = []
    collect_edges (node : nodes) = edges_for node nodes ++ collect_edges nodes

    fall :: [Node] -> SupportGraph -> SupportGraph
    fall []                graph = graph
    fall (node:rest_nodes) graph = 
      case safeMinimum $ map getLabel $ Graph.inwardEdges node graph of
        Nothing       -> fall rest_nodes graph
        Just 0        -> fall rest_nodes graph
        Just min_dist -> fall rest_nodes 
          $ Graph.adjustInwardEdgeLabels (+ negate min_dist) node
          $ Graph.adjustOutwardEdgeLabels (+ min_dist) node graph
  in
    Graph.deleteNode 0 $ Graph.filterEdges ((==0) . getLabel) $ fall (map fst all_nodes) $ Graph.fromEdges (collect_edges all_nodes)

isRemovable :: SupportGraph -> Node -> Bool
isRemovable graph node = and $ do
  -- each supported node ...
  supported_node <- Graph.successors node graph
  return $ or $ do
    -- must have at least one support node ...
    support_node <- Graph.predecessors supported_node graph
    -- other than `node` itself:
    return (node /= support_node)

countNodesSupportedBy :: SupportGraph -> Node -> Int
countNodesSupportedBy graph start_node = 
  let 
    go :: IntSet -> [Node] -> IntSet
    go collected nodes_to_check = 
      let 
        new_collected :: [Node]
        new_collected = do
          node <- nodes_to_check
          guard (node `IntSet.notMember` collected)
          guard (Graph.predecessorSet node graph `IntSet.isSubsetOf` collected)
          return node

        check_next :: [Node]
        check_next = do
          node <- new_collected
          Graph.successors node graph
      in
        if null new_collected then
          collected
        else
          go (collected <> IntSet.fromList new_collected) check_next

    supported_nodes :: IntSet
    supported_nodes = go (IntSet.singleton start_node) (Graph.successors start_node graph)
  in
    IntSet.size supported_nodes - 1

main :: IO ()
main = do
  input <- parseFile parser "input/22.txt"

  let support_graph = mkSupportGraph input

  putStr "Part 1: "
  print
    $ length
    $ filter (isRemovable support_graph)
    $ Graph.nodes support_graph

  putStr "Part 2: "
  print
    $ sum
    $ map (countNodesSupportedBy support_graph)
    $ Graph.nodes support_graph
