module Interpreter where

-- used in Day12 and Day23

import Utils (Parser)
import Text.Megaparsec (sepBy, choice, (<|>))
import Text.Megaparsec.Char (newline, string, hspace, lowerChar)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Sequence as S
import Data.List (iterate')
import Data.Foldable (toList)

-- cpy x y copies x (either an integer or the value of a register) into register y.
-- inc x increases the value of register x by one.
-- dec x decreases the value of register x by one.
-- jnz x y jumps to an instruction y away (positive means forward; negative means backward), but only if x is not zero.
-- tgl x toggles the instruction x away (positive means forward; negative means backward):

type Ref = Either Int Char

data Command
  = Cpy Ref Ref
  | Inc Ref
  | Dec Ref
  | Jnz Ref Ref
  | Tgl Ref
  | Mul Ref Ref
  | Add Ref Ref

instance Show Command where
  show cmd =
    case cmd of
      Cpy ref1 ref2 -> "cpy " <> show_ref ref1 <> " " <> show_ref ref2
      Jnz ref1 ref2 -> "jnz " <> show_ref ref1 <> " " <> show_ref ref2
      Mul ref1 ref2 -> "mul " <> show_ref ref1 <> " " <> show_ref ref2
      Add ref1 ref2 -> "add " <> show_ref ref1 <> " " <> show_ref ref2
      Inc ref -> "inc " <> show_ref ref
      Dec ref -> "dec " <> show_ref ref
      Tgl ref -> "tgl " <> show_ref ref
    where
      show_ref (Left val) = show val
      show_ref (Right reg) = [reg]

type Memory = M.Map Char Int

type Program = S.Seq Command

parser :: Parser Program
parser = S.fromList <$> command `sepBy` newline
  where
    command :: Parser Command
    command = choice [cpy, inc, dec, jnz, tgl, mul, add]

    integer :: Parser Int
    integer = signed hspace decimal

    ref :: Parser (Either Int Char)
    ref = (Left <$> integer) <|> (Right <$> lowerChar)

    inc = Inc <$ string "inc " <*> ref
    dec = Dec <$ string "dec " <*> ref
    jnz = Jnz <$ string "jnz " <*> ref <* string " " <*> ref
    cpy = Cpy <$ string "cpy " <*> ref <* string " " <*> ref
    mul = Mul <$ string "mul " <*> ref <* string " " <*> ref
    add = Add <$ string "add " <*> ref <* string " " <*> ref
    tgl = Tgl <$ string "tgl " <*> ref

data State = State
  { getProgram :: !Program
  , getMemory :: !Memory
  , getPointer :: !Int
  }

instance Show State where
  show (State program mem i) = show mem <> "\n\n" <> code
    where
      line_number j =
        replicate (length (show $ length program) - length (show j)) ' ' <> show j

      go j cmd = line_number j <> (if i == j then " > " else "   ") ++ show cmd

      code = unlines $ toList $ S.mapWithIndex go program

resolve :: Memory -> Either Int Char -> Int
resolve memory (Left value) = value
resolve memory (Right register) =
  M.findWithDefault 0 register memory

eval :: State -> State
eval state@(State program mem i) =
  case program S.!? i of
    Nothing                    -> state

    Just (Cpy ref (Right reg)) -> state
      { getMemory = M.insert reg (resolve mem ref) mem
      , getPointer = i+1 }

    Just (Inc (Right reg))     -> state
      { getMemory = M.adjust (+1) reg mem
      , getPointer = i+1 }

    Just (Dec (Right reg))     -> state
      { getMemory = M.adjust (\x -> x-1) reg mem
      , getPointer = i+1 }

    Just (Jnz ref1 ref2)       ->
      if resolve mem ref1 == 0 then
        state { getPointer = i+1 }
      else
        state { getPointer = i + resolve mem ref2 }

    Just (Tgl ref)             -> state { getPointer = i+1, getProgram = program' }
      where
        cmd_index = i + resolve mem ref
        toggled_cmd = toggle <$> program S.!? cmd_index
        program' = case toggled_cmd of
          Just cmd -> S.update cmd_index cmd program
          -- If an attempt is made to toggle an instruction 
          -- outside the program, nothing happens.
          Nothing  -> program

    Just (Mul ref (Right reg)) -> state 
      { getPointer = i+1
      , getMemory = M.adjust (* resolve mem ref) reg mem }

    Just (Add ref (Right reg)) -> state 
      { getPointer = i+1
      , getMemory = M.adjust (+ resolve mem ref) reg mem }

    -- If toggling produces an invalid instruction (like cpy 1 2) 
    -- and an attempt is later made to execute that instruction, 
    -- skip it instead.
    Just _ -> state { getPointer = i+1 }

isRunning :: State -> Bool
isRunning (State program _ i) =
  0 <= i && i < length program

run :: Program -> Memory -> State
run program mem = head
  $ dropWhile isRunning
  $ iterate eval
  $ State program mem 0

toggle :: Command -> Command
toggle cmd =
  case cmd of
    -- For one-argument instructions, inc becomes dec, 
    -- and all other one-argument instructions become inc.
    Inc ref -> Dec ref
    Dec ref -> Inc ref
    Tgl ref -> Inc ref
    -- For two-argument instructions, jnz becomes cpy, 
    -- and all other two-instructions become jnz.
    Jnz ref1 ref2 -> Cpy ref1 ref2
    Cpy ref1 ref2 -> Jnz ref1 ref2
    Mul ref1 ref2 -> Jnz ref1 ref2
    Add ref1 ref2 -> Jnz ref1 ref2