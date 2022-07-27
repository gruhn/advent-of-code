module Main where
import Text.Megaparsec (Parsec, some, sepBy, parse, errorBundlePretty)
import Data.Void (Void)
import Text.Megaparsec.Char (letterChar, hspace, string, newline)
import Text.Megaparsec.Char.Lexer (lexeme, decimal)
import Data.List (transpose)

type Parser = Parsec Void String

data Reindeer = R6r String Int Int Int
    deriving Show

parser :: Parser [[Int]]
parser = 
    let lex = lexeme hspace
        name = lex (some letterChar)
        str = lex . string

        pace :: Int -> Int -> Int -> [Int]
        pace kms run pause =
            cycle (replicate run kms ++ replicate pause 0)

        line = pace
            <$  name
            <*  str "can fly"
            <*> lex decimal
            <*  str "km/s for"
            <*> lex decimal
            <*  str "seconds, but then must rest for"
            <*> lex decimal
            <*  str "seconds."

    in  line `sepBy` newline

main :: IO ()
main = do
    input <- parse parser "" <$> readFile "2015/input/14.txt"
    case input of
        Left err -> putStr (errorBundlePretty err)
        Right reindeers -> do
            let race = scanl1 (+)

            putStr "Part 1: "
            print $ maximum 
                  $ (!! (2503+1)) . race <$> reindeers

            let winners scores = (== maximum scores) <$> scores
                countWins = length . filter id

            putStr "Part 2: "
            print $ maximum 
                  $ fmap countWins 
                  $ transpose . fmap winners . transpose 
                  $ take 2503 . race <$> reindeers