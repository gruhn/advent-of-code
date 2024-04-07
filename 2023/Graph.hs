module Graph
  ( Node
  , NodeSet
  , Edge(..)
  , Graph
  , empty
  , isEmpty
  , nodes
  , nodeSet
  , member
  , view
  , match
  , edges
  , insertNode
  , insertEdge
  , deleteNode
  , deleteEdge
  , fromEdges
  , lookupEdgeLabel
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
  , componentOutDegree
  , contractNode
  , stoerWagner
  ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.Maybe (maybeToList, fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (guard)
import Data.Function (on)
import Data.Foldable (maximumBy, for_)
import Test.QuickCheck (Arbitrary, (===), discard, forAll)
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Arbitrary (Arbitrary(arbitrary))
import Test.QuickCheck.Property (Property)
import Test.QuickCheck.Monadic (assert, PropertyM, monadicIO)
import qualified GHC.List as List

type Node = Int

type NodeSet = IntSet.IntSet

data Edge a = Edge
  { getSource :: Node
  , getTarget :: Node
  , getLabel  :: a
  } deriving Show

data Graph a = Graph
  { getPre :: IntMap NodeSet
  , getSuc :: IntMap NodeSet
  , getEdgeLabels :: Map (Node, Node) a
  } deriving Show

----------------------- INTERNAL -----------------------

modifySuc :: (IntMap NodeSet -> IntMap NodeSet) -> Graph a -> Graph a
modifySuc f graph = graph { getSuc = f (getSuc graph) }

modifyPre :: (IntMap NodeSet -> IntMap NodeSet) -> Graph a -> Graph a
modifyPre f graph = graph { getPre = f (getPre graph) }

modifyEdgeLabels :: (Map (Node, Node) a -> Map (Node, Node) a) -> Graph a -> Graph a
modifyEdgeLabels f graph = graph { getEdgeLabels = f (getEdgeLabels graph) }

deleteInwardEdges :: Node -> Graph a -> Graph a
deleteInwardEdges target graph =
  IntSet.fold (`deleteEdge` target) graph (predecessorSet target graph)

deleteOutwardEdges :: Node -> Graph a -> Graph a
deleteOutwardEdges source graph =
  IntSet.fold (source `deleteEdge`) graph (successorSet source graph)

----------------------- EXPORTED -----------------------

empty :: Graph a
empty = Graph IntMap.empty IntMap.empty Map.empty

isEmpty :: Graph a -> Bool
isEmpty = null . getPre

nodes :: Graph a -> [Node]
nodes = IntMap.keys . getPre

nodeSet :: Graph a -> NodeSet
nodeSet = IntMap.keysSet . getPre

member :: Node -> Graph a -> Bool
member node graph = IntMap.member node (getPre graph)

edges :: Graph a -> [Edge a]
edges = map to_edge . Map.toList . getEdgeLabels
  where
    to_edge :: ((Node, Node), a) -> Edge a
    to_edge ((source, target), label) = Edge source target label

-- | Insert a node without edges into the graph. If the node is already contained,
-- the original graph is returned.
insertNode :: Node -> Graph a -> Graph a
insertNode node =
    modifySuc (IntMap.insertWith (\_ old -> old) node IntSet.empty)
  . modifyPre (IntMap.insertWith (\_ old -> old) node IntSet.empty)

insertEdge :: Edge a -> Graph a -> Graph a
insertEdge = insertEdgeWith const

insertEdgeWith :: (a -> a -> a) -> Edge a -> Graph a -> Graph a
insertEdgeWith f (Edge source target label) =
    modifyEdgeLabels (Map.insertWith f (source, target) label)
  . modifySuc (IntMap.insertWith IntSet.union source (IntSet.singleton target))
  . modifyPre (IntMap.insertWith IntSet.union target (IntSet.singleton source))
  . insertNode source
  . insertNode target

-- | Delete a node and all connected edges. If the node is not contained in 
--   the graph the original graph is returned.
deleteNode :: Node -> Graph a -> Graph a
deleteNode node =
    modifySuc (IntMap.delete node)
  . modifyPre (IntMap.delete node)
  . deleteOutwardEdges node
  . deleteInwardEdges node

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

predecessorSet :: Node -> Graph a -> NodeSet
predecessorSet target = fromMaybe IntSet.empty . IntMap.lookup target . getPre

predecessors :: Node -> Graph a -> [Node]
predecessors node = IntSet.toList . predecessorSet node

successorSet :: Node -> Graph a -> NodeSet
successorSet source = fromMaybe IntSet.empty . IntMap.lookup source . getSuc

successors :: Node -> Graph a -> [Node]
successors node = IntSet.toList . successorSet node

neighborSet :: Node -> Graph a -> NodeSet
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

type Decomp a = (NodeSet, Node, NodeSet, Graph a)

view :: Node -> Graph a -> Decomp a
view node graph =
  ( predecessorSet node graph
  , node
  , successorSet node graph
  , deleteNode node graph
  )

match :: Graph a -> Maybe (Decomp a)
match graph = do
  (node, _) <- IntMap.lookupMin $ getPre graph
  return $ view node graph

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

componentOutDegree :: NodeSet -> Graph a -> Int
componentOutDegree component graph = length $ do
  nodeA <- IntSet.toList component
  nodeB <- successors nodeA graph
  guard $ nodeB `IntSet.notMember` component
  return nodeB

-- | 
contractNode :: forall a. Node -> Node -> Graph a -> Graph a
contractNode nodeA nodeB graph =
  let
    rename :: Node -> Node
    rename node = if node == nodeB then nodeA else node

    edges_renamed :: [Edge a]
    edges_renamed = do
      Edge source target label <- inwardEdges nodeB graph ++ outwardEdges nodeB graph
      -- don't add self-edges otherwise they are always added (TODO: should they?)
      guard $ source /= nodeA && target /= nodeA
      return $ Edge (rename source) (rename target) label
  in
    foldr insertEdge (deleteNode nodeB graph) edges_renamed

stoerWagner :: forall a. Graph a -> [NodeSet]
stoerWagner graph =
  case IntMap.lookupMin (getPre graph) of
    Nothing              -> []
    Just (start_node, _) ->
      let
        initial_surface :: IntMap Int
        initial_surface =
            IntMap.fromSet (const 1)
          $ IntSet.delete start_node
          $ successorSet start_node graph

        find_cut :: Node -> NodeSet -> IntMap Int -> Maybe (Node, Node, NodeSet)
        find_cut prev_node component surface =
          case IntMap.toList surface of
            [] -> Nothing
            [(last_node, _)] -> Just (prev_node, last_node, component)
            surface_list ->
              let
                (next_node, _) = maximumBy (compare `on` snd) surface_list
                component' = IntSet.insert next_node component
                surface' =
                    IntMap.delete next_node
                  $ IntMap.unionWith (+) surface
                  $ IntMap.fromSet (const 1)
                  $ successorSet next_node graph IntSet.\\ component
              in
                find_cut next_node component' surface'

        re_add :: Node -> Node -> NodeSet -> NodeSet
        re_add v1 v2 node_set
          | IntSet.member v1 node_set = IntSet.insert v2 node_set
          | otherwise = node_set
      in
        case find_cut start_node (IntSet.singleton start_node) initial_surface of
          Nothing -> []
          Just (v1, v2, vs) -> vs : map (re_add v1 v2) (stoerWagner (contractNode v1 v2 graph))

----------------------- TESTS -----------------------

instance Arbitrary a => Arbitrary (Edge a) where
  arbitrary = 
    Edge 
      <$> QuickCheck.chooseInt (0, 10) 
      <*> QuickCheck.chooseInt (0, 10) 
      <*> arbitrary

prop_invariant_pre_suc_keyset_match :: Graph () -> Property
prop_invariant_pre_suc_keyset_match graph = 
  IntMap.keysSet (getPre graph) === IntMap.keysSet (getSuc graph)

prop_clean_node_delete :: Property
prop_clean_node_delete = 
  forAll (arbitrary :: QuickCheck.Gen [Edge ()]) $ \graph_edges ->
    let graph = Graph.fromEdges graph_edges in
    case IntMap.lookupMin (getPre graph) of
      Nothing        -> discard -- empty graph
      Just (node, _) -> monadicIO $ do
        let graph' = Graph.deleteNode node graph

        for_ (IntMap.toList $ getPre graph') $ \(v, vs) -> do
          assert $ node /= v
          assert $ node `IntSet.notMember` vs

        for_ (IntMap.toList $ getSuc graph') $ \(v, vs) -> do
          assert $ node /= v
          assert $ node `IntSet.notMember` vs

        for_ (Map.keys $ getEdgeLabels graph') $ \(v1,v2) -> do
          assert $ node /= v1 
          assert $ node /= v2 

prop_node_contract :: Property
prop_node_contract = 
  forAll (arbitrary :: QuickCheck.Gen [Edge ()]) $ \case
    [] -> discard
    (Edge v1 v2 _ : _) | v1 == v2 -> discard
    (e:es) -> monadicIO $ do
      let Edge node1 node2 _ = e
          graph0 = Graph.fromEdges (e:es)
          graph1 = Graph.contractNode node1 node2 graph0

      for_ (IntMap.toList $ getPre graph1) $ \(v, vs) -> do
        assert $ node2 /= v
        assert $ node2 `IntSet.notMember` vs

      for_ (IntMap.toList $ getSuc graph1) $ \(v, vs) -> do
        assert $ node2 /= v
        assert $ node2 `IntSet.notMember` vs

      for_ (Map.keys $ getEdgeLabels graph1) $ \(v1,v2) -> do
        assert $ node2 /= v1 
        assert $ node2 /= v2 

