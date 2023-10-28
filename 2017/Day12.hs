module Main where
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (newline, string)
import Utils (Parser, parseFile, converge)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

parser :: Parser (IntMap IntSet)
parser = IntMap.fromList <$> line `sepBy` newline
 where
  line :: Parser (Int, IntSet)
  line = do
   from <- decimal
   string " <-> "
   to <- decimal `sepBy` string ", "
   return (from, IntSet.fromList to)

invert :: IntMap IntSet -> IntMap IntSet
invert edges = 
 IntMap.fromListWith (<>) $ do
  (from, tos) <- IntMap.toList edges
  to <- IntSet.toList tos
  return (to, IntSet.singleton from)
 
neighborsOf :: IntMap IntSet -> IntSet -> IntSet
neighborsOf edges nodes = 
 IntSet.unions $ do
  node <- IntSet.toList nodes
  return $ IntMap.findWithDefault IntSet.empty node edges

transitiveClosure :: IntMap IntSet -> IntSet -> IntSet
transitiveClosure edges = 
 converge (IntSet.union <*> neighborsOf edges)

partition :: IntMap IntSet -> [IntSet]
partition edges
 | null edges = []
 | otherwise = group : partition edges_without_group
 where
  (sample, _) = IntMap.findMin edges
  group = transitiveClosure edges (IntSet.singleton sample)
  edges_without_group = IntSet.fold IntMap.delete edges group

main :: IO ()
main = do
  edges_directed <- parseFile parser "input/12.txt"
  let edges = edges_directed <> invert edges_directed

  putStr "Part 1: "
  let group0 = transitiveClosure edges (IntSet.singleton 0)
  print $ IntSet.size group0

  putStr "Part 2: "
  let nodes_grouped = partition edges
  print $ length nodes_grouped
