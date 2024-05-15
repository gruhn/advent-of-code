module ParseUtils where

import Data.Void ( Void )
import Text.Megaparsec ( Parsec, parse, errorBundlePretty )
import Text.Megaparsec.Char ( hspace )
import Text.Megaparsec.Char.Lexer ( lexeme, signed, decimal )

type Parser = Parsec Void String

integer :: Parser Int
integer = signed hspace decimal

parseWith :: Parser a -> String -> IO a
parseWith parser file_path = do 
  input <- readFile file_path
  case parse parser "" input of
    Left err     -> error (errorBundlePretty err)
    Right output -> return output
