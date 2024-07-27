module Main where

import Data.Void ( Void )
import ParseUtils (Parser, parseWith, integer)
import Text.Megaparsec ( count )
import Text.Megaparsec.Char ( space, hspace )
import qualified Text.Megaparsec.Char.Lexer as Lex
import Data.Maybe (maybeToList)

data Tree = Node [Tree] [Int]

parser :: Parser Tree
parser = tree
  where
    decimal :: Parser Int
    decimal = Lex.lexeme space Lex.decimal

    node :: Parser Tree
    node = do
      child_count <- decimal
      meta_count <- decimal
      Node <$> count child_count node <*> count meta_count decimal

allMetadata :: Tree -> [Int]
allMetadata (Node children metadata) = 
  metadata ++ concatMap allMetadata children
  
treeValue :: Tree -> Int
treeValue (Node []       metadata) = sum metadata
treeValue (Node children metadata) = sum $ do
  index <- metadata
  child <- take 1 $ drop (index-1) children
  return $ treeValue child

main :: IO ()
main = do
  tree <- parseWith parser "input/08.txt"

  putStr "Part 1: "
  print $ sum $ allMetadata tree

  putStr "Part 2: "
  print $ treeValue tree
