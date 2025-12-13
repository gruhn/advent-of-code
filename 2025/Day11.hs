module Main (main) where
import Utils (Parser, parseFile, loeb, adjacentPairs)
import qualified Data.Map.Lazy as LazyMap
import Data.Map.Lazy (Map)
import Text.Megaparsec (sepEndBy, some, sepBy)
import Text.Megaparsec.Char (newline, lowerChar, string, char)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

type Graph = Map String (Set String)

parser :: Parser Graph
parser = LazyMap.fromList <$> line `sepEndBy` newline
  where
    node :: Parser String
    node = some lowerChar

    line :: Parser (String, Set String)
    line = do
      source_node <- node
      string ": "
      target_nodes <- node `sepBy` char ' '
      return (source_node, Set.fromList target_nodes)

edgesFrom :: Graph -> String -> [String]
edgesFrom graph node = maybe [] Set.toList (LazyMap.lookup node graph)

pathCounts :: Graph -> Map String (Map String Int)
pathCounts graph =
  let
    all_nodes = Set.unions (LazyMap.keysSet graph : LazyMap.elems graph)

    count_from :: String -> Map String (Map String Int) -> Map String Int
    count_from start path_counts =
      LazyMap.unionsWith (+) $ LazyMap.singleton start 1 : map (path_counts LazyMap.!) (edgesFrom graph start)
  in
    loeb $ LazyMap.fromSet count_from all_nodes

main :: IO ()
main = do
  graph <- parseFile parser "input/11.txt"

  let all_path_counts = pathCounts graph

      get_count :: (String, String) -> Int
      get_count (start, end) = fromMaybe 0 $ do
        reachable_nodes <- LazyMap.lookup start all_path_counts
        LazyMap.lookup end reachable_nodes

      tour_count :: [String] -> Int
      tour_count = product . map get_count . adjacentPairs

  putStr "Part 1: "
  print $ get_count ("you", "out")

  putStr "Part 2: "
  print $ tour_count ["svr", "dac", "fft", "out"] + tour_count ["svr", "fft", "dac", "out"]
