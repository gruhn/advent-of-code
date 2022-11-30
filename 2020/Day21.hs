module Main where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map 
import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec (many1, letter, sepBy, space, string, newline, sepEndBy)
import Data.List (find, sortBy, intersperse, intercalate)
import Data.Maybe (mapMaybe, isJust)
import Data.Function ( on )
import Data.Foldable (toList, minimumBy)
import Control.Monad (join)
import Data.Bifunctor (bimap)
import Control.Monad.IO.Class (liftIO)
import Data.Either.Extra (fromEither)

type Food = (Set String, Set String)

parser :: Parser [Food]
parser = food `sepBy` newline where
    word :: Parser String
    word = many1 letter

    ingredients :: Parser (Set String)
    ingredients = Set.fromList 
        <$> word `sepEndBy` space

    allergens :: Parser (Set String)
    allergens = Set.fromList 
        <$> word `sepBy` string ", "

    food :: Parser Food
    food = do
        ings <- ingredients
        string "(contains "
        algs <- allergens
        string ")"
        return (ings, algs)

makeGraph :: [Food] -> Map String (Set String)
makeGraph = foldr accum Map.empty where
    accum :: Food -> Map String (Set String) -> Map String (Set String)
    accum (ings, algs) graph = 
        foldr (\alg -> Map.insertWith Set.intersection alg ings) graph algs

matching :: (Ord a, Ord b) => Map a (Set b) -> Maybe (Map a b)
matching graph 
    | null graph = Just Map.empty 
    | otherwise  = 
        let excludeTarget b = Map.map (Set.filter (/= b)) 

            matchWith a graph b = Map.insert a b 
                <$> matching (excludeTarget b graph)

            (a, bs) = minimumBy (compare `on` length . snd) $ Map.toList graph
            graph' = Map.delete a graph

        in join . find isJust $ Set.map (matchWith a graph') bs

main :: IO ()
main = do
    input <- parseFromFile parser "input/21.txt"
    case input of 
        Left err -> print err
        Right foods -> do
            let graph = makeGraph foods

            putStr "Part 1: "
            let ys = Set.unions . Map.elems $ graph
            print $ sum $ fmap (Set.size . (Set.\\ ys) . fst) foods

            putStr "Part 2: "
            print $ showPart2 <$> matching graph

showPart2 :: Map String String -> String
showPart2 
    = intercalate "," 
    . map snd 
    . sortBy (compare `on` fst) 
    . Map.toList
