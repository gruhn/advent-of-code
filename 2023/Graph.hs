module Graph
  ( Node
  , Edge(..)
  , Graph
  , empty
  , nodes
  , edges
  , insertNode
  , insertEdge
  , deleteNode
  , deleteEdge
  , fromEdges
  , successors
  , successorSet
  , predecessors
  , predecessorSet
  , neighbors
  , neighborSet
  , inwardEdges
  , outwardEdges
  , modifyEdges
  , filterEdges
  , adjustEdgeLabel
  , adjustInwardEdgeLabels
  , adjustOutwardEdgeLabels
  , inDegree
  , outDegree
  , transitiveClosure
  ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (maybeToList, fromMaybe)
import Data.IntSet (IntSet)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (guard)

type Node = Int

data Edge a = Edge
  { getSource :: Node
  , getTarget :: Node
  , getLabel  :: a
  } deriving Show

data Graph a = Graph
  { getPre :: IntMap IntSet
  , getSuc :: IntMap IntSet 
  , getEdgeLabels :: Map (Node, Node) a
  }
  deriving Show

----------------------- INTERNAL -----------------------

modifySuc :: (IntMap IntSet -> IntMap IntSet) -> Graph a -> Graph a
modifySuc f graph = graph { getSuc = f (getSuc graph) }

modifyPre :: (IntMap IntSet -> IntMap IntSet) -> Graph a -> Graph a
modifyPre f graph = graph { getPre = f (getPre graph) }

modifyEdgeLabels :: (Map (Node, Node) a -> Map (Node, Node) a) -> Graph a -> Graph a
modifyEdgeLabels f graph = graph { getEdgeLabels = f (getEdgeLabels graph) }

deleteInwardEdges :: Node -> Graph a -> Graph a
deleteInwardEdges target graph =
  IntSet.fold (`deleteEdge` target) graph (predecessorSet target graph)

deleteOutwardEdges :: Node -> Graph a -> Graph a
deleteOutwardEdges source graph =
  IntSet.fold (deleteEdge source) graph (successorSet source graph)

----------------------- EXPORTED -----------------------

empty :: Graph a
empty = Graph IntMap.empty IntMap.empty Map.empty

nodes :: Graph a -> [Node]
nodes = IntMap.keys . getPre

edges :: Graph a -> [Edge a]
edges = map to_edge . Map.toList . getEdgeLabels
  where
    to_edge :: ((Node, Node), a) -> Edge a
    to_edge ((source, target), label) = Edge source target label

insertNode :: Node -> Graph a -> Graph a
insertNode node =
    modifySuc (IntMap.insertWith const node IntSet.empty)
  . modifyPre (IntMap.insertWith const node IntSet.empty)

insertEdge :: Edge a -> Graph a -> Graph a
insertEdge = insertEdgeWith const

insertEdgeWith :: (a -> a -> a) -> Edge a -> Graph a -> Graph a
insertEdgeWith f (Edge source target label) =
    modifyEdgeLabels (Map.insertWith f (source, target) label)
  . modifySuc (IntMap.insertWith IntSet.union source (IntSet.singleton target))
  . modifyPre (IntMap.insertWith IntSet.union target (IntSet.singleton source))

-- | Delete a node and all connected edges. If the node is not contained in 
--   the graph the original graph is returned.
deleteNode :: Node -> Graph a -> Graph a
deleteNode node =
    deleteOutwardEdges node
  . deleteInwardEdges node
  . modifySuc (IntMap.delete node)
  . modifyPre (IntMap.delete node)

deleteEdge :: Node -> Node -> Graph a -> Graph a
deleteEdge source target =
    modifyEdgeLabels (Map.delete (source, target))
  . modifySuc (IntMap.adjust (IntSet.delete target) source)
  . modifyPre (IntMap.adjust (IntSet.delete source) target)

adjustEdgeLabel :: (a -> a) -> Node -> Node -> Graph a -> Graph a
adjustEdgeLabel f source target = modifyEdgeLabels (Map.adjust f (source, target))

adjustOutwardEdgeLabels :: (a -> a) -> Node -> Graph a -> Graph a
adjustOutwardEdgeLabels f source graph = 
  IntSet.fold (adjustEdgeLabel f source) graph (successorSet source graph)

adjustInwardEdgeLabels :: (a -> a) -> Node -> Graph a -> Graph a
adjustInwardEdgeLabels f target graph = 
  IntSet.fold (\source -> adjustEdgeLabel f source target) graph (predecessorSet target graph)

fromEdges :: [Edge a] -> Graph a
fromEdges = foldr insertEdge empty

lookupEdgeLabel :: Node -> Node -> Graph a -> Maybe a
lookupEdgeLabel source target (Graph _ _ edge_labels) =
  Map.lookup (source, target) edge_labels

predecessorSet :: Node -> Graph a -> IntSet
predecessorSet target = fromMaybe IntSet.empty . IntMap.lookup target . getPre

predecessors :: Node -> Graph a -> [Node]
predecessors node = IntSet.toList . predecessorSet node

successorSet :: Node -> Graph a -> IntSet 
successorSet source = fromMaybe IntSet.empty . IntMap.lookup source . getSuc

successors :: Node -> Graph a -> [Node]
successors node = IntSet.toList . successorSet node

neighborSet :: Node -> Graph a -> IntSet
neighborSet node graph = successorSet node graph <> predecessorSet node graph

neighbors :: Node -> Graph a -> [Node]
neighbors node = IntSet.toList . neighborSet node

inwardEdges :: Node -> Graph a -> [Edge a]
inwardEdges target graph = do
  source <- predecessors target graph
  label  <- maybeToList $ lookupEdgeLabel source target graph
  return (Edge source target label)

outwardEdges :: Node -> Graph a -> [Edge a]
outwardEdges source graph = do
  target <- successors source graph
  label  <- maybeToList $ lookupEdgeLabel source target graph
  return (Edge source target label)

type Context a = ([(Node, a)], [(Node, a)])

context :: Node -> Graph a -> Context a
context node graph =
  let 
    inn = [ (source, label) | Edge source _ label <- inwardEdges node graph ]
    out = [ (target, label) | Edge _ target label <- outwardEdges node graph ]
  in
    (inn, out)

insertContext :: Node -> Context a -> Graph a -> Graph a
insertContext node (inn, out) graph = 
  let 
    inward_edges  = [ Edge source node label | (source, label) <- inn ]
    outward_edges = [ Edge node target label | (target, label) <- out ]
  in
    foldr insertEdge graph (inward_edges ++ outward_edges)

modifyEdges :: (Context a -> Context a) -> Node -> Graph a -> Graph a
modifyEdges f node graph = 
  insertContext node (f (context node graph)) $ deleteNode node graph

filterEdges :: forall a. (Edge a -> Bool) -> Graph a -> Graph a
filterEdges keep initial_graph = 
  let
    go :: Edge a -> Graph a -> Graph a
    go edge graph 
      | keep edge = graph
      | otherwise = deleteEdge (getSource edge) (getTarget edge) graph
  in
    foldr go initial_graph (edges initial_graph)

outDegree :: Node -> Graph a -> Int
outDegree node = IntSet.size . successorSet node

inDegree :: Node -> Graph a -> Int
inDegree node = IntSet.size . predecessorSet node

-- TODO: test this
transitiveClosure :: Graph Int -> Graph Int
transitiveClosure initial_graph = 
  let
    go :: [Edge Int] -> Graph Int -> Graph Int
    go [] graph = graph
    go (Edge source mid label1 : rest_edges) graph =
      let
        new_edges :: [Edge Int]
        new_edges = do
          guard (source /= mid)
          Edge _ target label2 <- outwardEdges mid graph
          return $ Edge source target (label1 + label2)
      in
        go (new_edges ++ rest_edges) (foldr (insertEdgeWith min) graph new_edges)
  in
    go (edges initial_graph) initial_graph
