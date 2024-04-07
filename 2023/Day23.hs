module Main where

import Utils (withCoords)
import Graph (Graph, Node, Edge(Edge), NodeSet)
import qualified Graph
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (maybeToList, fromJust)
import Control.Monad (guard)
import Data.Foldable (traverse_)
import Data.List (group)
import Data.Function ((&))
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.IntSet as IntSet
import Data.Either (partitionEithers)

type Pos = (Int,Int)

data Dir = Up | Dwn | Lft | Rgt 
  deriving Show

move :: Dir -> Pos -> Pos
move dir (x,y) = 
  case dir of
    Up  -> (x,y-1)
    Dwn -> (x,y+1)
    Lft -> (x-1,y)
    Rgt -> (x+1,y)

isDownHillMove :: Char -> Dir -> Bool
isDownHillMove source_tile dir = 
  case (source_tile, dir) of
    ('.', _  ) -> True
    ('^', Up ) -> True
    ('v', Dwn) -> True
    ('>', Rgt) -> True
    ('<', Lft) -> True
    _          -> False

contractGraph :: Graph Int -> Graph Int
contractGraph initial_graph =
  let
    go :: Node -> Graph Int -> Graph Int
    go node graph =
      case Graph.neighbors node graph of
        -- 0 neighbors ==> graph is disconnected (should not happen)
        []  -> undefined
        -- 1 neighbors ==> got start or target node
        [_] -> graph
        -- 2 neighbors ==> path node
        [neighborA, neighborB] ->
          let label = fromJust $ Graph.lookupEdgeLabel neighborA node graph in 
          graph
            & Graph.contractNode neighborA node
            & Graph.adjustEdgeLabel (+label) neighborA neighborB
            & Graph.adjustEdgeLabel (+label) neighborB neighborA
        -- >2 neighbors ==> intersection node
        _ -> graph
  in
    foldr go initial_graph $ Graph.nodes initial_graph

paths :: Node -> Node -> Graph Int -> [Int]
paths source target graph =
  let
    go :: Node -> NodeSet -> [Int]
    go current visited
      | current == target = [0]
      | otherwise = do
        Edge _ next dist <- Graph.outwardEdges current graph
        guard (IntSet.notMember next visited)
        path_length <- go next (IntSet.insert next visited)
        return (dist+path_length)
  in
    go source (IntSet.singleton source)

main :: IO ()
main = do
  input <- readFile "input/23.txt"

  let 
    grid :: Map Pos Char
    grid = Map.fromList $ do
      (pos, tile) <- withCoords $ lines input
      guard (tile /= '#')
      return (pos, tile)

    nodes :: Map Pos Node
    nodes = Map.fromList $ zip (Map.keys grid) [1..]

    (start_pos , _) = Map.findMin grid
    (target_pos, _) = Map.findMax grid

    start_node :: Node
    start_node = nodes Map.! start_pos

    target_node :: Node
    target_node = nodes Map.! target_pos

    (downhill_edges, uphill_edges) = 
      partitionEithers $ do
        (pos_from, node_from) <- Map.toList nodes
        dir <- [Up, Dwn, Lft, Rgt]
        let pos_to = move dir pos_from
        node_to <- maybeToList $ Map.lookup pos_to nodes
        let tile_from = grid Map.! pos_from
        if isDownHillMove tile_from dir then
          return $ Left $ Edge node_from node_to 1
        else
          return $ Right $ Edge node_from node_to 1

  putStr "Part 1: "
  print 
    $ maximum
    $ paths start_node target_node 
    $ Graph.fromEdges downhill_edges

  putStrLn "Part 2: "
  traverse_ print 
    $ map head . group 
    $ scanl max 0
    $ paths start_node target_node 
    $ contractGraph
    $ Graph.fromEdges (downhill_edges ++ uphill_edges)
