module Main where

import qualified Data.Map as Map
import Text.Megaparsec (Parsec, some, sepBy, parse, errorBundlePretty)
import Data.Void (Void)
import Text.Megaparsec.Char (alphaNumChar, newline, char)
import Control.Arrow (second)

parser :: Parsec Void String (Map.Map String String)
parser =
    let code = some alphaNumChar
        pair = do
            parent <- code
            char ')'
            child <- code
            return (child, parent)
        pairs = pair `sepBy` newline
    in  Map.fromList <$> pairs

path :: Map.Map String String -> String -> String -> [String]
path dict = go [] where
    go steps from to
        | from == to = steps
        | otherwise  = 
            let prev = dict Map.! to
            in  go (prev : steps) from prev

main :: IO ()
main = do
    input <- parse parser "" <$> readFile "2019/input/06.txt"
    case input of 
        Left error -> putStr (errorBundlePretty error)
        Right dict -> do
            putStr "Part 1: "
            let allPaths = path dict "COM" <$> Map.keys dict
            print $ sum . fmap length $ allPaths

            putStr "Part 2: "
            let path1 = path dict "COM" "YOU"
                path2 = path dict "COM" "SAN"

                commonPrefix = length
                    $ takeWhile (uncurry (==))
                    $ zip path1 path2

            print $ length path1 + length path2 - 2 * commonPrefix