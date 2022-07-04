module Main where

import ParseUtil (splitOn)
import Data.Char (isUpper, isLower)
import qualified Data.Map as Map
import Data.List (nub, delete)

type Cave = String
type Edge = (Cave, Cave)
type CaveSystem = Map.Map Cave [Cave]

parse :: String -> Maybe CaveSystem
parse input = do
    let forwardEdges = map (splitOn "-") . lines $ input
        backwardEdges = map reverse forwardEdges
    edges <- mapM parseEdge (forwardEdges ++ backwardEdges)
    let caves = nub . map fst $ edges
        adjacent cave = map snd . filter ((==cave) . fst) $ edges
        assoc = map (\cave -> (cave, adjacent cave)) caves
    Just $ Map.map (delete "end") $ Map.delete "start" $ Map.fromList assoc

parseEdge :: [String] -> Maybe Edge
parseEdge [a,b] = Just (a,b)
parseEdge _     = Nothing

main :: IO ()
main = do
    caveSystem <- parse <$> readFile "2021/12-input.txt"
    putStr "Part 1: "
    print $ fmap (length . paths1) caveSystem 
    putStr "Part 2: "
    print $ fmap (length . nub . paths2) caveSystem 

connections :: CaveSystem -> Cave -> [Cave]
connections system cave = 
    Map.findWithDefault [] cave system

isSmall :: Cave -> Bool
isSmall cave =
    cave /= "start" && cave /= "end" && all isLower cave

visited :: Cave -> CaveSystem -> CaveSystem
visited cave system
    | isSmall cave = Map.map (delete cave) system
    | otherwise    = system

paths1 :: CaveSystem -> [[Cave]]
paths1 system = go system [] "end"
    where
        go system path "start" = ["start":path]
        go system path cave = 
            case connections system cave of
                []    -> []
                conns -> concatMap (go (visited cave system) (cave:path)) conns
    
-- absolutely terrible performance
paths2 :: CaveSystem -> [[Cave]]
paths2 system = go system [] False "end"
    where
        go system path doubleVisit "start" = ["start":path]
        go system path doubleVisit cave = 
            case connections system cave of
                []    -> []
                conns -> 
                    if not doubleVisit && isSmall cave 
                        then concatMap (go (visited cave system) (cave:path) False) conns ++ concatMap (go system (cave:path) True) conns
                        else concatMap (go (visited cave system) (cave:path) doubleVisit) conns

