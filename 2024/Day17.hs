module Main (main) where
import Utils (Parser, integer, parseFile, converge)
import Text.Megaparsec.Char (string, newline, char)
import Text.Megaparsec (sepBy)
import Data.Bits (xor)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Foldable (traverse_, find, toList)
import Debug.Trace (traceShowId, traceShowM)
import Data.Maybe (maybeToList)
import Control.Monad (guard, when)
import Data.List (isPrefixOf)
import Data.Containers.ListUtils (nubOrd)

data State = State
  { registerA :: Int
  , registerB :: Int
  , registerC :: Int
  , output    :: [Int]
  , program   :: Seq Int
  , pointer   :: Int
  } deriving (Eq, Show)

initialState :: Int -> Int -> Int -> [Int] -> State
initialState regA regB regC instrs = 
  State 
    { registerA = regA
    , registerB = regB
    , registerC = regC
    , pointer   = 0
    , output    = []
    , program   = Seq.fromList instrs
    }

parser :: Parser State
parser = do
  regA <- string "Register A: " *> integer <* newline
  regB <- string "Register B: " *> integer <* newline
  regC <- string "Register C: " *> integer <* newline
  newline
  instrs <- string "Program: " *> integer `sepBy` char ','
  newline
  return $ initialState regA regB regC instrs

comboOperand :: State -> Int
comboOperand s
  -- Combo operands 0 through 3 represent literal values 0 through 3.
  | 0 <= operand s && operand s <= 3 = operand s
  -- Combo operand 4 represents the value of register A.
  | operand s == 4 = s.registerA
  -- Combo operand 5 represents the value of register B.
  | operand s == 5 = s.registerB
  -- Combo operand 6 represents the value of register C.
  | operand s == 6 = s.registerC
  -- Combo operand 7 is reserved and will not appear in valid programs.
  | operand s == 7 = error "combo operand 7"
  | otherwise      = error $ "combo operand " ++ show (operand s)

incPointer :: State -> State 
incPointer state = state { pointer = state.pointer + 2 }

getInstr :: State -> Maybe Int
getInstr state = Seq.lookup state.pointer state.program

operand :: State -> Int
operand state = state.program `Seq.index` (state.pointer + 1)

step :: State -> State
step state = 
  case getInstr state of 
    -- The adv instruction (opcode 0) performs division. 
    -- The numerator is the value in the A register. 
    -- The denominator is found by raising 2 to the power of the instruction's combo operand. 
    -- (So, an operand of 2 would divide A by 4 (2^2); an operand of 5 would divide A by 2^B.) 
    -- The result of the division operation is truncated to an integer and then written to the 
    -- A register.
    Just 0 -> incPointer $ state { registerA = state.registerA `div` (2 ^ comboOperand state) }
    -- The bxl instruction (opcode 1) calculates the bitwise XOR of register B and the instruction's 
    -- literal operand, then stores the result in register B.
    Just 1 -> incPointer $ state { registerB = state.registerB `xor` operand state}
    -- The bst instruction (opcode 2) calculates the value of its combo operand modulo 8 (thereby 
    -- keeping only its lowest 3 bits), then writes that value to the B register.
    Just 2 -> incPointer $ state { registerB = comboOperand state `mod` 8 }
    -- The jnz instruction (opcode 3) does nothing if the A register is 0. However, if the A 
    -- register is not zero, it jumps by setting the instruction pointer to the value of its 
    -- literal operand; if this instruction jumps, the instruction pointer is not increased by 2 
    -- after this instruction.
    Just 3 | state.registerA == 0 -> incPointer state
           | otherwise            -> state { pointer = operand state }
    -- The bxc instruction (opcode 4) calculates the bitwise XOR of register B and register C, 
    -- then stores the result in register B. (For legacy reasons, this instruction reads an 
    -- operand but ignores it.)
    Just 4 -> incPointer $ state { registerB = state.registerB `xor` state.registerC }
    -- The out instruction (opcode 5) calculates the value of its combo operand modulo 8, 
    -- then outputs that value. (If a program outputs multiple values, they are separated by 
    -- commas.)
    Just 5 -> incPointer $ state { output = (comboOperand state `mod` 8) : state.output }
    -- The bdv instruction (opcode 6) works exactly like the adv instruction except that the 
    -- result is stored in the B register. (The numerator is still read from the A register.)
    Just 6 -> incPointer $ state { registerB = state.registerA `div` (2 ^ comboOperand state) }
    -- The cdv instruction (opcode 7) works exactly like the adv instruction except that the 
    -- result is stored in the C register. (The numerator is still read from the A register.)
    Just 7 -> incPointer $ state { registerC = state.registerA `div` (2 ^ comboOperand state) }
    Just c -> error $ "undefined opcode: " ++ show c
    Nothing -> state

run :: State -> [State]
run state = 
  if (step state).pointer == state.pointer then
    [state]
  else
    state : run (step state)

search :: State -> Int
search state = head $ do
  let ints = map (2^) [0 ..] :: [Integer]
  regA <- map fromIntegral $ takeWhile (\i -> i <= fromIntegral (maxBound :: Int)) ints
  let state' = state { registerA = regA }
      p_list = toList state.program
  traceShowM regA
  let stateN = last $ takeWhile (\s -> reverse s.output `isPrefixOf` p_list) $ run state'
  when (length stateN.output > 4) (traceShowM (regA, stateN.output))
  guard $ reverse stateN.output == toList state.program
  return regA

main :: IO ()
main = do
  state0 <- parseFile parser "input/17.txt"

  putStr "Part 1: "
  print $ reverse $ output $ last $ run state0
  
  -- print $ reverse $ output $ last $ run state0
  -- traverse_ print $ run $ state0 { registerA = 8404168381 }
  -- print $ search state0
