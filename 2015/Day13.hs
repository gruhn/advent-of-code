module Main where

import Text.Megaparsec (Parsec, some, sepBy, parse, errorBundlePretty, (<|>), many, skipMany)
import Data.Void (Void)
import Text.Megaparsec.Char.Lexer (lexeme, decimal)
import Text.Megaparsec.Char (letterChar, string, newline, hspace, char)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (delete, maximumBy)
import Data.Function ((&), on)
import Data.Functor ((<&>))

type Parser = Parsec Void String
type Edge = Set.Set String
type Graph = Map.Map Edge Int

edge :: String -> String -> Edge
edge v w = Set.fromList [v,w]

singleton :: String -> Int -> String -> Graph
singleton v weight w = 
    Map.singleton (edge v w) weight

parser :: Parser Graph
parser = 
    let lex = lexeme hspace
        string' w = lex (string w)
        person = lex (some letterChar)

        gain = 1 <$ string' "gain"
        lose = (-1) <$ string' "lose"

        happiness :: Parser Int
        happiness = (*) 
            <$> (gain <|> lose)
            <*> lex decimal 

        edgeP :: Parser Graph
        edgeP = singleton
            <$> person 
            <*  string' "would" 
            <*> happiness
            <*  string' "happiness units by sitting next to"
            <*> person
            <*  char '.'

        edges = edgeP `sepBy` newline

    in  Map.unionsWith (+) <$> edges

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

        closeTour [] = []
        closeTour (start:rest) = [start] ++ rest ++ [start] 

    in  concatMap (goNext vs) vs <&> closeTour

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
    input <- parse parser "" <$> readFile "input/13.txt"
    case input of
        Left err -> putStr (errorBundlePretty err)
        Right graph -> do
            putStr "Part 1: "
            print $ tours graph <&> pathCost graph & maximum

            let extraEdges = singleton "Me" 0 <$> nodes graph
                graph' = Map.unions (graph:extraEdges)

            putStr "Part 2: "
            print $ tours graph' <&> pathCost graph' & maximum