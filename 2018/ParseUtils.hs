module ParseUtils where

import Data.Void ( Void )
import Text.Megaparsec ( Parsec, parse, errorBundlePretty )
import Text.Megaparsec.Char ( hspace )
import Text.Megaparsec.Char.Lexer ( lexeme, signed, decimal )

type Parser = Parsec Void String

integer :: Parser Int
integer = signed hspace decimal

parseHardError :: Parser a -> String -> a
parseHardError parser input =  
    case parse parser "" input of
        Left err -> error (errorBundlePretty err)
        Right output -> output
