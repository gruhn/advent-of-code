{-# LANGUAGE TupleSections #-}
module Main where
import Data.Tree (Forest, unfoldForest, Tree (Node))
import Data.List ((\\))
import Data.Function ( (&) )
import Data.Char ( ord )

-- >>> ['a' .. 'z'] \\ "iol"
-- "abcdefghjkmnpqrstuvwxyz"

passwordSpace :: Forest Char
passwordSpace =
    let alphabet = ['a' .. 'z'] \\ "iol"

        constrainDepth :: Int -> Forest Char -> Forest Char
        constrainDepth 0 _  = []
        constrainDepth _ [] = []
        constrainDepth d (Node c children : siblings) = 
            Node c (constrainDepth (d-1) children) : constrainDepth d siblings

        isStraight :: String -> Bool
        isStraight [] = True
        isStraight [c] = True
        isStraight (c1:c2:cs) = 
            ord c1 + 1 == ord c2 && isStraight (c2:cs)

        constrainStraight :: String -> Forest Char -> Forest Char
        constrainStraight str forest
            | isStraight str = forest
            | otherwise =
                let go (Node c children) = Node c $
                        constrainStraight (tail str ++ [c]) children
                in  go <$> forest

        -- constrainPairs :: Forest Char -> Forest Char

        space = unfoldForest (,alphabet) alphabet
            & constrainDepth 8
            & constrainStraight "___"

    in  space

-- >>> passwords passwordSpace & drop 200 & take 10
-- ["jt","ju","jv","jw","jx","jy","jz","ka","kb","kc"]

passwords :: Forest Char -> [String]
passwords [] = [""]
passwords nodes =
    let go (Node c children) = (c:) <$> passwords children
    in  concatMap go nodes

main :: IO ()
main = do
    let oldPass = "vzbxkghb"

    putStrLn "Part 1: "
