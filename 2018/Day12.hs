module Main (main) where
import Data.List (tails)
import Data.Map (Map)
import qualified Data.Map as Map
import ParseUtils (Parser, parseWith)
import Text.Megaparsec.Char (string, newline)
import Control.Applicative (many)
import Text.Megaparsec (oneOf, sepEndBy)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Control.Monad (guard)

windows :: forall a. Int -> [a] -> [[a]]
windows len = map (take len) . takeWhile ((>= len) . length) . tails

type State = (Int, String)

padLeft :: State -> State
padLeft (offset, str) =
  let
    dot_count_left = length $ takeWhile (=='.') str
    padding = 4 - dot_count_left
  in
    (offset - padding, replicate padding '.' ++ str)

padRight :: State -> State
padRight (offset, str) =
  let
    dot_count_right = length $ takeWhile (=='.') $ reverse str
    padding = 4 - dot_count_right
  in
    (offset, str ++ replicate padding '.')

padEnds :: State -> State
padEnds = padRight . padLeft

step :: Map String Char -> State -> State
step rules state =
  let
    go :: String -> Char
    go pattern@[_,_,_,_,_] =
      fromMaybe '.' (Map.lookup pattern rules)
    go _ = error "pattern with length =/= 5"

    (offset, str) = padEnds state
  in
    (offset+2, map go $ windows 5 str)

parser :: Parser (String, Map String Char)
parser =
  let
    initial_state :: Parser String
    initial_state = do
      string "initial state: "
      many (oneOf ".#")

    rule :: Parser (String, Char)
    rule = do
      source <- many (oneOf "#.")
      string " => "
      target <- oneOf "#."
      return (source, target)

    rules :: Parser (Map String Char)
    rules = Map.fromList <$> rule `sepEndBy` newline
  in
    (,) <$> initial_state <* newline <* newline <*> rules

indexSum :: State -> Int
indexSum (offset, str) = sum $ do 
  (i, char) <- zip [offset..] str 
  guard $ char == '#'
  return i

showState :: State -> String
showState (offset, str) = replicate (10 + offset) '.' ++ str

main :: IO ()
main = do
  (str_0, rules) <- parseWith parser "input/12.txt"
  let state_0 = (0, str_0)

  traverse_ (putStrLn . showState) 
    $ take 120
    $ iterate (step rules) state_0

  putStr "Part 1: "
  let state_20 = iterate (step rules) state_0 !! 20
  print $ indexSum state_20

  putStr "Part 2: "
  -- after ~120 generations the chaos seems to subside and the indices are 
  -- always shifted by +1 for each following generation:
  let state_120 = iterate (step rules) state_0 !! 120
  print $ indexSum (fst state_120 - 120 + 50_000_000_000, snd state_120)
