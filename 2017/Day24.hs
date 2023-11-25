module Main where
import Utils (Parser, parseFile, iterateJust, maximalBy)
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (newline, char)
import Text.Megaparsec.Char.Lexer (decimal)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe (listToMaybe)
import Control.Monad (guard)
import Data.Function (on)

type Edge = (Int,Int)
type Path = NonEmpty Int
type Graph = IntMap (Set Path)

parser :: Parser [Edge]
parser = edge `sepEndBy` newline
  where
    edge :: Parser Edge
    edge = (,) <$> decimal <* char '/' <*> decimal

insertEdge :: Edge -> Graph -> Graph
insertEdge (v,w) =
    IntMap.insertWith (<>) v (Set.singleton (NonEmpty.singleton w))
  . IntMap.insertWith (<>) w (Set.singleton (NonEmpty.singleton v))

deletePath :: Int -> Path -> Graph -> Graph
deletePath left (right :| right_to_left) =
    IntMap.filter (not . null)
  . IntMap.adjust (Set.delete (right :| right_to_left)) left
  . IntMap.adjust (Set.delete (left :| reverse right_to_left)) right

findLinearPath :: Graph -> Maybe (Path, Int, Path)
findLinearPath graph = listToMaybe $ do
  (node, paths) <- IntMap.toList $ IntMap.filter ((== 2) . length) graph
  return $ case Set.toList paths of
    [left_path, right_path] -> (left_path, node, right_path)
    _ -> error "node should have exactly two associated paths"

chainLinearPaths :: Graph -> Graph
chainLinearPaths = last . iterateJust go
  where
    go :: Graph -> Maybe Graph
    go graph = do
      (left_path, mid, right_path) <- findLinearPath graph

      -- Have to hard-code the special case that the start node `0` is never
      -- chained. Otherwise, the start node can get removed, if it happens to
      -- have exactly two edges:
      guard $ mid /= 0

      let adjust_left :: Path -> Path
          adjust_left path 
            | NonEmpty.head path == mid = right_path <> path
            | otherwise = path

          adjust_right :: Path -> Path
          adjust_right path 
            | NonEmpty.head path == mid = left_path <> path
            | otherwise = path

      return 
        $ IntMap.adjust (Set.map adjust_left) (NonEmpty.head left_path)
        $ IntMap.adjust (Set.map adjust_right) (NonEmpty.head right_path)
        $ IntMap.delete mid graph

expand :: Path -> Graph -> [(Path, Graph)]
expand (node :| _) graph = do
  path <- Set.toList $ IntMap.findWithDefault Set.empty node graph
  return (path, deletePath node path graph)

maximalPaths :: Path -> Graph -> [Path]
maximalPaths  start_path graph = 
  let paths = expand start_path graph in
  if null paths then
    return start_path
  else do
    (path, graph') <- paths
    map (<> start_path) (maximalPaths path graph')

strength :: Path -> Int
strength (v :| vs) = sum $ zipWith (+) (v:vs) vs

maxStrength :: [Path] -> Int
maxStrength = maximum . map strength

main :: IO ()
main = do
  edges <- parseFile parser "input/24.txt"

  let graph = chainLinearPaths $ foldr insertEdge IntMap.empty edges
      start = NonEmpty.singleton 0
      maximal_paths = maximalPaths start graph

  putStr "Part 1: "
  print $ maxStrength maximal_paths

  putStr "Part 2: "
  print $ maxStrength $ maximalBy (compare `on` length) maximal_paths
