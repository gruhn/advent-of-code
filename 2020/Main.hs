module Main where

import Day10 (parser, solver)
import Data.Bifunctor (Bifunctor(bimap))
import Text.Parsec.String (parseFromFile)

fromEither :: Either a a -> a
fromEither (Left a) = a
fromEither (Right a) = a

main :: IO ()
main = do
    input <- parseFromFile parser "input/10.txt"
    fromEither $ bimap print solver input