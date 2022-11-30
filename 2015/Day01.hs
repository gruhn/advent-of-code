module Main where
import Text.Megaparsec (Parsec, (<|>), many, parse)
import Data.Void (Void)
import Text.Megaparsec.Char (char)

parser :: Parsec Void String [Int]
parser = 
    let up = 1 <$ char '('
        down = -1 <$ char ')'
    in  many (up <|> down)

main :: IO ()
main = do
    input <- parse parser "" <$> readFile "input/01.txt"

    putStr "Part 1: "
    print $ sum <$> input

    putStr "Part 2: "
    print $ length . takeWhile (>= 0) . scanl (+) 0 <$> input