{-# LANGUAGE TupleSections #-}
module Main where
import Data.Tree (Forest, unfoldForest, Tree (Node, subForest, rootLabel), drawForest)
import Data.List ((\\))
import Data.Function ( (&) )
import Data.Char (ord)
import Control.Arrow (second)
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)

modifyChildren :: (Forest a -> Forest a) -> Tree a -> Tree a
modifyChildren f (Node label children) =
    Node label (f children)

passwordSpace :: Forest Char
passwordSpace =
    let alphabet = ['a'..'z'] \\ "iol"

        pruneLength :: Int -> Forest Char -> Forest Char
        pruneLength 0 _ = []
        pruneLength d forest = 
            modifyChildren (pruneLength (d-1)) <$> forest

        isStraight :: String -> Bool
        isStraight [] = True
        isStraight [c] = True
        isStraight (c1:c2:cs) = 
            ord c1 + 1 == ord c2 && isStraight (c2:cs)

        pruneStraight :: String -> Forest Char -> Forest Char
        pruneStraight cs forest = 
            let go :: Tree Char -> (Forest Char -> Forest Char)
                go (Node c children)
                    | isStraight (tail cs ++ [c]) = (Node c children :)
                    | null children = id
                    | otherwise = 
                        case pruneStraight (tail cs ++ [c]) children of
                            []        -> id
                            children' -> (Node c children' :)
            in  forest & fmap go & foldr ($) []

        isPair :: String -> Bool
        isPair [c1,c2] = c1 == c2
        isPair _ = False

        prunePairs :: String -> Int -> Forest Char -> Forest Char
        prunePairs cs 0 forest = forest
        prunePairs cs n forest =
            let go :: Tree Char -> Maybe (Tree Char)
                go (Node c children) =
                    let cs' = take 2 (cs ++ [c])
                    in  if isPair cs' && n == 1 then
                            Just $ Node c children
                        else if isPair cs' && n > 1 then
                            case prunePairs "" (n-1) children of
                                [] -> Nothing
                                children' -> Just $ Node c children'
                        else
                            case prunePairs cs' n children of
                                [] -> Nothing
                                children' -> Just $ Node c children'
            in  mapMaybe go forest

        space = unfoldForest (,alphabet) alphabet
            & pruneLength 8
            & ("vzbxkghb" .<=)
            & pruneStraight "___"
            & prunePairs "" 2

    in  space

passwords :: Forest Char -> [String]
passwords nodes =
    let go (Node c [])       = [[c]]
        go (Node c children) = (c:) <$> passwords children
    in  concatMap go nodes

(.<=) :: String -> Forest Char -> Forest Char
(.<=) [] forest = forest
(.<=) (c:cs) forest = 
    case dropWhile ((< c) . rootLabel) forest of
        [] -> []
        (node:nodes) ->
            modifyChildren (cs .<=) node : nodes

main :: IO ()
main = do
    putStrLn "Part 1: "
    print $ passwords passwordSpace & take 10

    putStrLn "Part 2: "
