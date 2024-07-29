module ParseUtils where

import Data.Void ( Void )
import Text.Megaparsec ( Parsec, parse, errorBundlePretty, eof )
import Text.Megaparsec.Char ( hspace )
import qualified Text.Megaparsec.Char.Lexer as Lex

type Parser = Parsec Void String

integer :: Parser Int
integer = Lex.signed hspace Lex.decimal

symbol :: String -> Parser String
symbol = Lex.symbol hspace

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme hspace

parseWith :: Parser a -> String -> IO a
parseWith parser file_path = do 
  input <- readFile file_path
  case parse (parser <* eof) "" input of
    Left err     -> error (errorBundlePretty err)
    Right output -> return output
