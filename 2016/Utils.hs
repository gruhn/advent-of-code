module Utils (Parser, parseHardError, converge) where

import Text.Megaparsec (Parsec, parse, errorBundlePretty)
import Data.Void (Void)

type Parser = Parsec Void String

parseHardError :: Parser a -> String -> a
parseHardError parser input = 
  case parse parser "" input of
    Left  err    -> error (errorBundlePretty err)
    Right output -> output

converge :: Eq a => (a -> a) -> a -> a
converge f a
    | a == f a  = a
    | otherwise = converge f (f a)