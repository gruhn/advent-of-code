module Main (main) where

import Utils (Parser, parseFile, loeb)
import Text.Megaparsec (some, choice, sepEndBy)
import Text.Megaparsec.Char (string, newline, char, alphaNumChar)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Control.Monad (guard)

data Operator = AND | XOR | OR
  deriving (Show, Eq)

data Rule = Rule Operator String String  
  deriving (Show, Eq)

type RuleMap = Map String Rule

type ValueMap = Map String Bool

parser :: Parser (ValueMap, RuleMap)
parser =
  let
    bit :: Parser Bool
    bit = choice
      [ False <$ char '0'
      , True  <$ char '1'
      ]

    fact :: Parser (String, Bool)
    fact = do
      var <- some alphaNumChar
      string ": "
      value <- bit
      return (var, value)

    operator :: Parser Operator
    operator = choice
      [ AND <$ string " AND "
      , XOR <$ string " XOR "
      , OR  <$ string " OR "
      ]

    rule :: Parser (String, Rule)
    rule = do
      input1 <- some alphaNumChar
      op     <- operator
      input2 <- some alphaNumChar
      string " -> "
      output <- some alphaNumChar
      return (output, Rule op input1 input2)
  in do
    facts <- Map.fromList <$> fact `sepEndBy` newline
    newline
    rules <- Map.fromList <$> rule `sepEndBy` newline
    return (facts, rules)

evalOp :: Operator -> Bool -> Bool -> Bool
evalOp AND = (&&)
evalOp XOR = (/=)
evalOp OR  = (||)

evalRule :: Rule -> Map String Bool -> Bool
evalRule (Rule op var1 var2) values = evalOp op (values Map.! var1) (values Map.! var2)

eval :: ValueMap -> RuleMap -> ValueMap
eval inputs rules = loeb $ Map.union (Map.map const inputs) (Map.map evalRule rules)

fromBinary :: [Bool] -> Int
fromBinary []         = 0
fromBinary (bit:bits) = fromEnum bit + 2 * fromBinary bits

getOutput :: ValueMap -> [Bool]
getOutput values = do
  (name, value) <- Map.toAscList values
  guard $ head name == 'z'
  return value

main :: IO ()
main = do
  (facts, rules) <- parseFile parser "input/24.txt"

  putStr "Part 1: "
  print $ fromBinary $ getOutput $ eval facts rules
