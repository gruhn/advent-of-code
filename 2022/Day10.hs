module Main where
import Utils (Parser, parseHardError, chunksOf)
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (newline, string, hspace)
import Control.Applicative ((<|>))
import Text.Megaparsec.Char.Lexer ( signed, decimal )

data Instr = Addx Int | Noop
  deriving Show

parser :: Parser [Instr]
parser = instr `sepBy` newline
  where
    integer :: Parser Int
    integer = signed hspace decimal

    instr = addx <|> noop
    addx = Addx <$ string "addx " <*> integer
    noop = Noop <$ string "noop"

eval :: Int -> Instr -> Int
eval x Noop = x
eval x (Addx val) = x+val

-- just add a Noop before every Addx so we can pretend we have one instruction per cycle
expand :: [Instr] -> [Instr]
expand program = do
  instr <- program
  case instr of
    Addx val -> [Noop, Addx val]
    Noop     -> [Noop]

run :: [Instr] -> [Int]
run = scanl eval 1 . expand

pixel :: Int -> Int -> Char
pixel i x
  | abs (x-i) <= 1 = '#'
  | otherwise      = '.'

main :: IO ()
main = do
  program <- parseHardError parser <$> readFile "input/10.txt"
  let states = run program

  putStr "Part 1: "
  let is_middle_cycle (i, _) = (i-20) `mod` 40 == 0
      middle_cycles = filter is_middle_cycle $ zip [1..] states
  print $ sum $ uncurry (*) <$> middle_cycles

  putStrLn "Part 2:"
  let pixel_positions = (`mod` 40) <$> [0..]
      pixels = uncurry pixel <$> zip pixel_positions states
  putStrLn $ unlines $ chunksOf 40 pixels