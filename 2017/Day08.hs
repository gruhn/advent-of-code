{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main where

import Utils (Parser, parseFile, lexeme, symbol, integer, maximumBounded)
import Text.Megaparsec (sepBy, (<|>), choice, some)
import Text.Megaparsec.Char (newline, lowerChar)
import Data.Map (Map)
import qualified Data.Map as Map

type Condition = (String, Int -> Bool)

data Instr = Instr String Int Condition

parser :: Parser [Instr]
parser = instr `sepBy` newline
 where
  instr :: Parser Instr
  instr = Instr <$> register <*> update <*> condition

  register = lexeme (some lowerChar)

  inc =   1  <$ symbol "inc"
  dec = (-1) <$ symbol "dec"

  update = do
   sign <- inc <|> dec
   num <- integer
   return (sign*num)

  condition = (,) <$ symbol "if" <*> register <*> predicate

  relation = choice 
   [ (>=) <$ symbol ">="
   , (<=) <$ symbol "<="
   , (==) <$ symbol "=="
   , (/=) <$ symbol "!="
   , (>)  <$ symbol ">"
   , (<)  <$ symbol "<"
   ]

  predicate = do 
   rel <- relation
   num <- integer
   return (`rel` num)

type Memory = Map String Int

checkCondition :: Condition -> Memory -> Bool
checkCondition (reg, predicate) mem = 
 predicate $ Map.findWithDefault 0 reg mem

eval :: Memory -> Instr -> Memory
eval mem (Instr reg num cond)
 | checkCondition cond mem = Map.insertWith (+) reg num mem
 | otherwise = mem

main :: IO ()
main = do 
 instrs <- parseFile parser "./input/08.txt"

 let steps = scanl eval Map.empty instrs
 let max_reg_per_step = map maximumBounded steps

 putStr "Part 1: "
 print $ last max_reg_per_step 

 putStr "Part 2: "
 print $ maximum max_reg_per_step 
