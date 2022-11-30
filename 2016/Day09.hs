module Main where
import Utils (Parser, parseHardError)
import Text.Megaparsec (between, MonadParsec (lookAhead, eof, parseError), (<|>), manyTill, anySingle, many, anySingleBut, some, count, someTill, parse, ParseErrorBundle (bundleErrors))
import Text.Megaparsec.Char (char, upperChar)
import Text.Megaparsec.Char.Lexer (decimal)
import Control.Monad (void)
import qualified Data.List.NonEmpty as NE

data AST = Leaf String | Node Int Int [AST]
  deriving Show

parser :: Parser [AST]
parser = many (node <|> leaf)
  where
    marker :: Parser (Int, Int)
    marker = between open_par close_par pair
      where
        open_par  = char '('
        close_par = char ')'
        pair = (,) <$> decimal <* char 'x' <*> decimal

    node :: Parser AST
    node = do 
      (length, repeat) <- marker
      data_ahead <- count length anySingle

      case parse parser "" data_ahead of
        Left err -> parseError $ NE.head (bundleErrors err)
        Right segments -> 
          return $ Node length repeat segments

    leaf :: Parser AST
    leaf = Leaf <$> some upperChar

flatLength :: [AST] -> Int
flatLength = sum . fmap go_level_0
  where
    go_level_0 :: AST -> Int
    go_level_0 (Leaf str) = length str
    go_level_0 (Node _ repeat sub_trees) = 
      repeat * sum (go_level_n <$> sub_trees)

    go_level_n :: AST -> Int
    go_level_n (Leaf str) = length str
    go_level_n (Node len repeat sub_trees) = 
      let digitCount = length . show
          marker_length = length "(x)" + digitCount len + digitCount repeat
      in  marker_length + sum (go_level_n <$> sub_trees)

deepLength :: [AST] -> Int
deepLength = sum . fmap go
  where
    go :: AST -> Int
    go (Leaf str) = length str
    go (Node _ repeat sub_trees) =
      repeat * deepLength sub_trees

main :: IO ()
main = do
  input <- parseHardError parser <$> readFile "input/09.txt"

  putStr "Part 1: "
  print $ flatLength input

  putStr "Part 2: "
  print $ deepLength input