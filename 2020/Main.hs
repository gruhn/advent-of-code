module Main where

import Day11 (parser, solver)
import Data.Bifunctor (Bifunctor(bimap))
import Text.Parsec.String (parseFromFile)

fromEither :: Either a a -> a
fromEither (Left a) = a
fromEither (Right a) = a

main :: IO ()
main = do
    input <- parseFromFile parser "input/11.txt"
    fromEither $ bimap print solver input