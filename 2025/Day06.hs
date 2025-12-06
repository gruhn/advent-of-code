module Main (main) where
import Utils (Parser, parseFile, parse, splitBy)
import Text.Megaparsec (many, endBy, (<|>), some, optional)
import Text.Megaparsec.Char (char, newline, hspace, digitChar)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.List (transpose)

type Op = [Int] -> Int

inputFileParser :: Parser ([String], [Op])
inputFileParser = do
  number_rows <- num_row `endBy` newline
  operators <- many (op <* hspace)
  optional newline
  return (number_rows, operators)
  where
    num_row :: Parser String
    num_row = some (digitChar <|> char ' ')

    op :: Parser Op
    op = (product <$ char '*') <|> (sum <$ char '+')

parseColumns1 :: [String] -> [[Int]]
parseColumns1 rows = transpose $ do
  let row_parser = hspace *> many (decimal <* hspace)
  parse row_parser <$> rows

parseColumns2 :: [String] -> [[Int]]
parseColumns2 rows = do
  column <- splitBy (all (==' ')) $ transpose rows
  return $ parse (hspace *> decimal <* hspace) <$> column

main :: IO ()
main = do
  (number_rows, operators) <- parseFile inputFileParser "input/06.txt"

  putStr "Part 1: "
  print $ sum $ do
    (eval, column) <- zip operators (parseColumns1 number_rows)
    return $ eval column

  putStr "Part 2: "
  print $ sum $ do
    (eval, column) <- zip operators (parseColumns2 number_rows)
    return $ eval column
