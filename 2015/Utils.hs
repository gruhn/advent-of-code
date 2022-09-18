{-# LANGUAGE ScopedTypeVariables #-}

module Utils (distinct, splitOn, parseHardError, Parser) where

import Test.QuickCheck

import Data.List (isSuffixOf, stripPrefix, delete, isInfixOf)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Void (Void)
import Text.Megaparsec (Parsec, parse, errorBundlePretty)

-- stripSuffix

prop_stripSuffix :: String -> String -> Bool
prop_stripSuffix prefix suffix =
    stripSuffix (prefix ++ suffix) suffix == Just prefix

prop_stripSuffix' :: String -> String -> Bool
prop_stripSuffix' str1 str2 =
    str1 `isSuffixOf` str2 == isJust (stripSuffix str2 str1) 

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix as [] = Just as
stripSuffix [] _  = Nothing
stripSuffix (a:as) suf
    | length (a:as) > length suf = (a :) <$> stripSuffix as suf
    | (a:as) == suf = Just []
    | otherwise = Nothing

-- splitOn

prop1_splitOn :: String -> String -> Bool
prop1_splitOn list inf = 
    -- Splitting and concatenating leaves the list unchanged.
    -- In particular, multiple matches in a row are preserved.
    concat (splitOn list inf) == list

prop2_splitOn :: Int -> String -> Bool
prop2_splitOn n str = 
    -- Similar to prop1_splitOn but more specific, with a 
    -- string only composed of matches.
    let rep = replicate n str
    in  rep == concat rep `splitOn` str

prop3_splitOn :: String -> String -> Bool
prop3_splitOn str inf = 
    inf `isInfixOf` str 
    || str == ""
    || str `splitOn` inf == [str]

splitOn :: forall a. Eq a => [a] -> [a] -> [[a]]
splitOn list inf = go list []
    where
        inf' = reverse inf

        go :: [a] -> [a] -> [[a]]
        go [] [] = []
        go [] pre = [reverse pre]
        go (a:as) pre =
            case stripPrefix inf' (a:pre) of
                Nothing  -> go as (a:pre)
                Just []  -> inf : go as []
                Just suf -> reverse suf : inf : go as []

-- distinct

prop1_distinct :: String -> Bool
prop1_distinct str = 
    all (`elem` str) (distinct str)

prop2_distinct :: String -> Char -> String -> Bool
prop2_distinct pre c suf = 
    let noDups = distinct (pre ++ [c] ++ suf)
    in  c `notElem` delete c noDups

distinct :: Ord a => [a] -> [a]
distinct = Set.toList . Set.fromList

-- parseHardError

type Parser = Parsec Void String

parseHardError :: Parser a -> String -> a
parseHardError parser input =  
    case parse parser "" input of
        Left err -> error (errorBundlePretty err)
        Right output -> output
