module Main (main) where
import Utils (Parser, parseFile)
import Text.Megaparsec (sepEndBy, some, endBy, choice)
import Text.Megaparsec.Char (newline, char)
import Control.Monad (guard)
import Data.List (partition)

type Schematic = [Bool]

isLock :: Schematic -> Bool
isLock = and . take 5

overlap :: Schematic -> Schematic -> Bool
overlap lock key = or $ zipWith (&&) lock key

parser :: Parser [Schematic]
parser = schematic `sepEndBy` newline
 where
    cell :: Parser Bool
    cell = choice [ True <$ char '#', False <$ char '.' ]

    schematic :: Parser Schematic
    schematic = concat <$> some cell `endBy` newline

main :: IO ()
main = do
  (locks, keys) <- partition isLock <$> parseFile parser "input/25.txt"

  putStr "Part 1: "
  print $ length $ do
    lock <- locks
    key  <- keys
    guard $ not $ overlap lock key
