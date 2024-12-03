module Main (main) where

import Utils (Parser, integer, parseFile, matchAll)
import Text.Megaparsec.Char (string)
import Text.Megaparsec (choice)

data Instr = Mul Int Int | Do | Dont
  deriving (Eq, Show)

parser :: Parser [Instr]
parser = matchAll instr
  where
    instr :: Parser Instr
    instr = choice [instr_do, instr_dont, instr_mul]

    instr_do :: Parser Instr
    instr_do = Do <$ string "do()"

    instr_dont :: Parser Instr
    instr_dont = Dont <$ string "don't()"

    instr_mul :: Parser Instr
    instr_mul = do
      string "mul("
      a <- integer
      string ","
      b <- integer
      string ")"
      return $ Mul a b

skipDontBlocks :: [Instr] -> [Instr]
skipDontBlocks []  = []
skipDontBlocks ins = 
  let (do_block, rest) = span (/= Dont) ins   
  in  do_block ++ skipDontBlocks (dropWhile (/= Do) rest)

main :: IO ()
main = do
  instructions <- parseFile parser "input/03.txt"

  putStr "Part 1: "
  print $ sum [ a*b | Mul a b <- instructions ]

  putStr "Part 2: "
  print $ sum [ a*b | Mul a b <- skipDontBlocks instructions ]
