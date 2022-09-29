module Main where
import Utils (Parser, parseHardError)
import Text.Megaparsec (sepBy, choice, (<|>))
import Text.Megaparsec.Char (newline, string, hspace, lowerChar)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import qualified Data.Map as M
import qualified Data.Vector as V

-- cpy x y copies x (either an integer or the value of a register) into register y.
-- inc x increases the value of register x by one.
-- dec x decreases the value of register x by one.
-- jnz x y jumps to an instruction y away (positive means forward; negative means backward), but only if x is not zero.

data Command
  = Cpy (Either Int Char) Char 
  | Inc Char 
  | Dec Char
  | Jnz (Either Int Char) Int
  deriving Show

type Memory = M.Map Char Int

type Program = V.Vector Command

parser :: Parser Program
parser = V.fromList <$> command `sepBy` newline
  where
    command :: Parser Command
    command = choice [cpy, inc, dec, jnz]

    integer :: Parser Int
    integer = signed hspace decimal

    inc = Inc <$ string "inc " <*> lowerChar
    dec = Dec <$ string "dec " <*> lowerChar

    jnz = Jnz <$ string "jnz " <*> (jnz_val <|> jnz_reg) <* string " " <*> integer
      where
        jnz_val = Left  <$> integer
        jnz_reg = Right <$> lowerChar

    cpy = Cpy <$ string "cpy " <*> (cpy_val <|> cpy_reg) <* string " " <*> lowerChar
      where
        cpy_val = Left  <$> integer
        cpy_reg = Right <$> lowerChar

type State = (Memory, Int)

resolve :: Memory -> Either Int Char -> Int
resolve memory (Left value) = value
resolve memory (Right register) = 
  M.findWithDefault 0 register memory

eval :: Program -> State -> State
eval program (mem, i) =
  case program V.!? i of
    Just (Cpy ref reg) -> (M.insert reg (resolve mem ref) mem, i+1)
    Just (Inc reg)     -> (M.adjust (+1) reg mem, i+1)
    Just (Dec reg)     -> (M.adjust (\x -> x-1) reg mem, i+1)
    Just (Jnz ref val) -> 
      if resolve mem ref == 0 then
        (mem, i+1)
      else 
        (mem, i+val)
    Nothing            -> (mem, i)

isRunning :: Program -> State -> Bool
isRunning program (_, i) =
  0 <= i && i < length program

run :: Program -> State -> State
run program start_state = head
  $ dropWhile (isRunning program)
  $ iterate (eval program) start_state

main :: IO ()
main = do
  program <- parseHardError parser <$> readFile "2016/input/12.txt"

  putStr "Part 1: "
  print $ run program (mempty, 0)

  putStr "Part 2: "
  print "figured out manually"