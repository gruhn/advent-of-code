module Main where
import Utils (Parser, parseHardError)
import Text.Megaparsec.Char (newline, char)
import Text.Megaparsec ( sepBy, between, count )
import Text.Megaparsec.Char.Lexer (symbol, decimal)
import Control.Applicative ((<|>))
import Data.Foldable (for_, traverse_)
import Control.Monad (guard)
import Data.List (elemIndex, sort)

data Packet = Val Int | List [Packet]
  deriving (Show, Eq)

instance Ord Packet where
  -- If both values are integers, the lower integer should come first. 
  -- If the left integer is lower than the right integer, the inputs are in the right order. 
  -- If the left integer is higher than the right integer, the inputs are not in the right order. 
  -- Otherwise, the inputs are the same integer; continue checking the next part of the input.
  compare (Val l) (Val r) = compare l r
  -- If exactly one value is an integer, convert the integer to a list which contains that integer as its only value, then retry the comparison. 
  compare (Val l) (List rs) = compare (List [Val l]) (List rs)
  compare (List ls) (Val r) = compare (List ls) (List [Val r])
  -- If both values are lists, compare the first value of each list, then the second value, and so on. 
  -- If the left list runs out of items first, the inputs are in the right order. 
  -- If the right list runs out of items first, the inputs are not in the right order. 
  -- If the lists are the same length and no comparison makes a decision about the order, continue checking the next part of the input.
  compare (List ls) (List rs) = compare ls rs

parser :: Parser [(Packet, Packet)]
parser = pair `sepBy` count 2 newline
  where
    pair :: Parser (Packet, Packet)
    pair = (,) <$> packet <* newline <*> packet

    packet :: Parser Packet
    packet = list <|> val

    val :: Parser Packet
    val = Val <$> decimal

    list :: Parser Packet
    list = List <$> between (char '[') (char ']') (packet `sepBy` char ',')

main :: IO ()
main = do
  input <- parseHardError parser <$> readFile "input/13.txt"

  putStr "Part 1: "
  print $ sum $ do
    (index, (left, right)) <- zip [1..] input
    guard (left <= right)
    return index

  putStr "Part 2: "
  let divider_packet1 = List [List [Val 2]]
      divider_packet2 = List [List [Val 6]]

      (left_packets, right_packets) = unzip input
      
      all_packets = sort $ [divider_packet1, divider_packet2] <> left_packets <> right_packets

  print $ do 
    index1 <- elemIndex divider_packet1 all_packets
    index2 <- elemIndex divider_packet2 all_packets
    return $ (index1+1) * (index2+1)