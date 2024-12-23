module Main (main) where
import Utils (Parser, parseFile)
import Text.Megaparsec (some, sepEndBy)
import Text.Megaparsec.Char (lowerChar, string, newline)
import Control.Monad (guard)
import Data.Map.Strict (Map)
import Data.List (isPrefixOf, intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)

type Graph = Map String (Set String)

fromEdges :: [(String, String)] -> Graph
fromEdges edges = Map.fromListWith Set.union $ do
  (source, target) <- edges
  [ (source, Set.singleton target), (target, Set.singleton source) ]

parser :: Parser Graph
parser = fromEdges <$> edge `sepEndBy` newline
  where
    edge :: Parser (String, String)
    edge = do
      from <- some lowerChar
      string "-"
      to  <- some lowerChar
      return (from, to)

isClique :: Graph -> Set String -> Bool
isClique graph nodes = and $ do
  node <- Set.toList nodes
  return $ Set.isSubsetOf nodes $ Set.insert node (graph Map.! node)

sublists :: Int -> [a] -> [[a]]
sublists size list 
  | size <  0 = undefined
  | size == 0 = [[]]
  | otherwise = 
    case list of 
      []     -> []
      (a:as) -> [ a:as' | as' <- sublists (size-1) as ] ++ sublists size as

subsets :: Ord a => Int -> Set a -> [Set a]
subsets size = map Set.fromList . sublists size . Set.toList

deleteNode :: String -> Graph -> Graph
deleteNode node graph = 
  case Map.lookup node graph of
    Nothing        -> graph
    Just neighbors -> 
      Map.delete node $ foldr (Map.adjust (Set.delete node)) graph neighbors

maxOutDegree :: Graph -> Int
maxOutDegree = maximum . Map.map length

cliques :: Graph -> Int -> [Set String]
cliques graph clique_size =  
  case Map.lookupMax graph of
    Nothing                -> []
    Just (node, neighbors) -> 
      let
        cliques_with_node :: [Set String]
        cliques_with_node = do 
          subset <- subsets (clique_size-1) neighbors
          guard $ isClique graph subset
          return $ Set.insert node subset

        cliques_without_node :: [Set String]
        cliques_without_node =
          cliques (deleteNode node graph) clique_size
      in
        cliques_with_node ++ cliques_without_node

formatAnswer :: Set String -> String
formatAnswer = intercalate "," . Set.toAscList

main :: IO ()
main = do
  graph <- parseFile parser "input/23.txt"

  putStr "Part 1: "
  print $ length $ do 
    clique <- cliques graph 3
    guard $ any ("t" `isPrefixOf`) clique

  putStr "Part 2: "
  putStrLn $ formatAnswer $ head $ do 
    clique_size <- reverse [3 .. maxOutDegree graph]
    cliques graph clique_size
