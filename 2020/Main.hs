module Main where

import Day11 (parser, solver)
import Data.Bifunctor (Bifunctor(bimap))
import Text.Megaparsec (parse, errorBundlePretty)

fromEither :: Either a a -> a
fromEither (Left a) = a
fromEither (Right a) = a

main :: IO ()
main = do
    input <- parse parser "" <$> readFile "input/11.txt"

    case input of 
        Left error -> putStr (errorBundlePretty error)
        Right parsed -> solver parsed