module Utils (Parser, parseHardError) where

import Text.Megaparsec (Parsec, parse, errorBundlePretty)
import Data.Void (Void)

type Parser = Parsec Void String

parseHardError :: Parser a -> String -> a
parseHardError parser input = 
  case parse parser "" input of
    Left  err    -> error (errorBundlePretty err)
    Right output -> output
