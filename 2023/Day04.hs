module Main where
import Utils (Parser, parseFile, lexeme, symbol)
import qualified Data.List as List
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Control.Monad (guard)
import Control.Applicative (many)

type Card = (Int, [Int], [Int])

parser :: Parser [Card]
parser = card `sepEndBy` newline
  where
    card :: Parser Card
    card = do
      card_id <- symbol "Card" *> decimal <* symbol ":"
      left_numbers  <- many (lexeme decimal)
      symbol "|"
      right_numbers <- many (lexeme decimal)
      return (card_id, left_numbers, right_numbers)

matchCount :: Card -> Int
matchCount (_, left_nums, right_nums) = 
  length $ List.intersect left_nums right_nums

points :: Card -> Int
points card
  | matchCount card == 0 = 0
  | otherwise            = 2 ^ (matchCount card - 1)

spawnCounts :: [Card] -> [Int]
spawnCounts [] = []
spawnCounts (card : following_cards) = 
  let following_spawn_counts = spawnCounts following_cards
      card_spawn_count = 1 + sum (take (matchCount card) following_spawn_counts)
   in card_spawn_count : following_spawn_counts

main :: IO ()
main = do
  cards <- parseFile parser "input/04.txt"

  putStr "Part 1: "
  print $ sum $ map points cards

  putStr "Part 2: "
  print $ sum $ spawnCounts cards
