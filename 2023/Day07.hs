module Main where
import Utils (Parser, parseFile, mostCommon)
import Text.Megaparsec (sepEndBy, some)
import Text.Megaparsec.Char (newline, alphaNumChar, char)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.List (sort, group, sortOn, findIndex, elemIndex, maximumBy)
import Control.Arrow (second, Arrow (..))
import Data.Maybe (fromJust, fromMaybe)
import Data.Function (on)

parser :: Parser [(String, Int)]
parser = line `sepEndBy` newline
  where
    line :: Parser (String, Int)
    line = do
      hand <- some alphaNumChar 
      char ' '
      bid  <- decimal
      return (hand, bid)

handType :: String -> Int
handType hand =
  case sort $ map length $ group $ sort hand of
    [5]         -> 6 -- five of a kind
    [1,4]       -> 5 -- four of a kind
    [2,3]       -> 4 -- full house
    [1,1,3]     -> 3 -- three of a kind
    [1,2,2]     -> 2 -- two pair
    [1,1,1,2]   -> 1 -- one pair
    [1,1,1,1,1] -> 0 -- high card
    _           -> undefined

cardValue1 :: Char -> Int
cardValue1 card = fromJust $ elemIndex card "23456789TJQKA"

cardValue2 :: Char -> Int
cardValue2 card = fromJust $ elemIndex card "J23456789TQKA"

replaceJokers :: String -> String
replaceJokers hand = map replace hand
  where
    -- Pick most common card that is not itself a joker. 
    -- If all cards are jokers, default to the highest valued card: Ace.
    replacement_card :: Char
    replacement_card = fromMaybe 'A' (mostCommon $ filter (/= 'J') hand)

    replace :: Char -> Char
    replace 'J'  = replacement_card
    replace card = card

totalWinnings :: Ord a => (String -> a) -> [(String,Int)] -> Int
totalWinnings hand_value hands_and_bids = sum $ do
  let bids_ranked = map snd $ sortOn (hand_value . fst) hands_and_bids
  (rank, bid) <- zip [1..] bids_ranked
  return (rank * bid)

main :: IO ()
main = do
  input <- parseFile parser "input/07.txt"

  let hand_value1 :: String -> (Int, [Int])
      hand_value1 hand = (handType hand, map cardValue1 hand)

  putStr "Part 1: "
  print $ totalWinnings hand_value1 input

  let hand_value2 :: String -> (Int, [Int])
      hand_value2 hand = (handType (replaceJokers hand), map cardValue2 hand)

  putStr "Part 2: "
  print $ totalWinnings hand_value2 input
