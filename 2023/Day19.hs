module Main where

import Utils (Parser, parseFile)
import Text.Megaparsec (some, sepBy, choice, sepEndBy, endBy, MonadParsec (try), (<|>))
import Text.Megaparsec.Char (char, lowerChar, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (negate)
import Control.Monad (guard)

data Result = Accepted | Rejected | Continue String
  deriving (Show, Eq)

type Rule = (Char, Bound, Result)

data Workflow = Workflow 
  { getRules    :: [Rule]
  , getFallback :: Result 
  } 

data Bound = LessThan Int | GreaterThan Int | LessEquals Int | GreaterEquals Int

negate :: Bound -> Bound
negate = \case
  LessThan      value -> GreaterEquals value
  LessEquals    value -> GreaterThan   value
  GreaterEquals value -> LessThan      value
  GreaterThan   value -> LessEquals    value

type Rating = Map Char Int

parser :: Parser (Map String Workflow, [Rating])
parser = (,) <$> workflows <* newline <*> ratings
  where
    workflows :: Parser (Map String Workflow)
    workflows = Map.fromList <$> workflow `endBy` newline

    ratings :: Parser [Rating]
    ratings = rating `sepEndBy` newline

    workflow_name :: Parser String
    workflow_name = some lowerChar

    workflow :: Parser (String, Workflow)
    workflow = do
      name  <- workflow_name
      char '{'
      rules    <- try rule `endBy` char ','
      fallback <- result
      char '}'
      return (name, Workflow rules fallback)

    rule :: Parser Rule
    rule = (,,) 
      <$> lowerChar 
      <*> bound
      <*  char ':' <*> result
    
    bound :: Parser Bound
    bound = choice [ LessThan <$ char '<', GreaterThan <$ char '>' ] <*> decimal

    result :: Parser Result
    result = choice
      [ Accepted <$  char 'A'
      , Rejected <$  char 'R' 
      , Continue <$> workflow_name 
      ]

    rating :: Parser Rating
    rating = Map.fromList
      <$  char '{' 
      <*> (assignment `sepBy` char ',') 
      <*  char '}'

    assignment :: Parser (Char, Int)
    assignment = do
      var <- lowerChar
      char '='
      value <- decimal
      return (var, value)

type Range = (Int, Int)

nonEmpty :: Range -> Bool
nonEmpty (lower, upper) = lower <= upper

contains :: [Range] -> Int -> Bool
contains ranges value = any in_range ranges
  where 
    in_range :: Range -> Bool
    in_range (lower, upper) = lower <= value && value <= upper

width :: [Range] -> Int
width = sum . map go
  where
    go :: Range -> Int
    go (lower, upper) = upper - lower + 1

constrain :: Bound -> [Range] -> [Range]
constrain bound = filter nonEmpty . map go
  where
    go :: Range -> Range
    go (lower, upper) = case bound of
      LessThan      value -> (lower, min upper (value-1))
      LessEquals    value -> (lower, min upper value)
      GreaterThan   value -> (max lower (value+1), upper)
      GreaterEquals value -> (max lower value, upper)

check :: Map String Workflow -> Map Char [Range] -> Result -> [Map Char [Range]]
check workflows ranges = \case
  Accepted -> [ranges]
  Rejected -> []
  Continue workflow_name -> go ranges rules
    where
      Workflow rules fallback = workflows Map.! workflow_name

      go :: Map Char [Range] -> [Rule] -> [Map Char [Range]]
      go ranges' [] = check workflows ranges' fallback
      go ranges' ((var, bound, result) : rest_rules) =
        let ranges_sat = Map.adjust (constrain bound) var ranges'
            ranges_unsat = Map.adjust (constrain (negate bound)) var ranges'
         in check workflows ranges_sat result <|> go ranges_unsat rest_rules

isContainedIn :: Map Char Int -> Map Char [Range] -> Bool
isContainedIn rating ranges = and $ do 
  (var, value) <- Map.toList rating
  let range = ranges Map.! var
  return $ range `contains` value

combinations :: Map Char [Range] -> Int
combinations = product . Map.map width

main :: IO ()
main = do
  (workflows, ratings) <- parseFile parser "input/19.txt"

  let initial_ranges = Map.fromList [ (var, [(1,4000)]) | var <- "xmas" ]
      constrained_ranges = check workflows initial_ranges (Continue "in")

  putStr "Part 1: "
  print $ sum $ do
    rating <- ratings
    guard $ any (rating `isContainedIn`) constrained_ranges
    return $ sum rating

  putStr "Part 2: "
  print $ sum $ map combinations constrained_ranges
