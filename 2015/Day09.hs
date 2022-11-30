module Main where
import Text.Megaparsec (Parsec, some, sepBy, parse, errorBundlePretty)
import Data.Void (Void)
import qualified Data.Map as Map
import Text.Megaparsec.Char.Lexer (lexeme, decimal)
import Text.Megaparsec.Char (hspace, letterChar, string, newline)
import qualified Data.Set as Set
import Data.List (delete)
import Data.Function ((&))
import Data.Functor ((<&>))

type Parser = Parsec Void String

type Edge = Set.Set String

type Graph = Map.Map Edge Int

edge :: String -> String -> Edge
edge v w = Set.fromList [v,w]

parser :: Parser Graph
parser = 
    let edgeP :: Parser (Edge, Int)
        edgeP = do
            start <- some letterChar
            string " to "
            end <- some letterChar
            string " = "
            cost <- decimal
            return (edge start end, cost)

        edgesP = edgeP `sepBy` newline

    in  Map.fromList <$> edgesP

nodes :: Graph -> [String]
nodes graph =
    let edges = Map.keys graph
    in  edges & Set.unions & Set.toList

tours :: Graph -> [[String]]
tours graph =
    let hasEdge v w = Map.member (edge v w) graph
        
        go :: String -> [String] -> [[String]]
        go v [] = [[v]] 
        go v vs = 
            let neighbors = filter (hasEdge v) vs
            in  (v:) <$> concatMap (goNext vs) neighbors

        goNext :: [String] -> String -> [[String]]
        goNext vs v = go v (delete v vs)
        
        vs = nodes graph
    in  concatMap (goNext vs) vs

pathEdges :: [String] -> [Edge]
pathEdges nodes = 
    zip nodes (tail nodes) <&> uncurry edge

pathCost :: Graph -> [String] -> Int
pathCost graph path = 
    let edges = pathEdges path
        cost edge = graph Map.! edge
    in  edges <&> cost & sum

main :: IO ()
main = do
    input <- parse parser "" <$> readFile "input/09.txt"
    case input of
        Left err -> putStr (errorBundlePretty err)
        Right graph -> do
            putStr "Part 1: "
            print $ tours graph <&> pathCost graph & minimum

            putStr "Part 2: "
            print $ tours graph <&> pathCost graph & maximum