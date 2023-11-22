module Main where
import Utils (Parser, parseFile, integer, lexeme, symbol, count)
import Text.Megaparsec.Char (newline, lowerChar)
import Text.Megaparsec (choice, (<|>), sepEndBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Prelude hiding (read)

type Value = Either Char Int

data Instr =
    Set Char Value
  | Sub Char Value
  | Mul Char Value
  | Jnz Value Value
  deriving Show

parser :: Parser (Seq Instr)
parser = Seq.fromList <$> instr `sepEndBy` newline
  where
    value :: Parser (Either Char Int)
    value = (Left <$> lexeme lowerChar) <|> (Right <$> integer)

    instr :: Parser Instr
    instr = choice 
      [ Set <$ symbol "set" <*> lexeme lowerChar <*> value
      , Sub <$ symbol "sub" <*> lexeme lowerChar <*> value
      , Mul <$ symbol "mul" <*> lexeme lowerChar <*> value
      , Jnz <$ symbol "jnz" <*> value <*> value
      ]

data State = State 
  { getMemory :: Map Char Int 
  , getOffset :: Int
  } deriving (Show, Eq)

modifyMemory :: (Map Char Int -> Map Char Int) -> State -> State
modifyMemory f state = state { getMemory = f (getMemory state) }

modifyOffset :: (Int -> Int) -> State -> State
modifyOffset f state = state { getOffset = f (getOffset state) }

readValue :: Value -> State -> Int
readValue (Right val) _ = val
readValue (Left reg) state = 
  Map.findWithDefault 0 reg (getMemory state)

step :: Seq Instr -> State -> Maybe State
step instrs state = do
  let read val = readValue val state
  instr <- instrs Seq.!? getOffset state
  case instr of
    -- set X Y sets register X to the value of Y.
    Set x y -> 
      return $ modifyOffset (+ 1) $ modifyMemory (Map.insert x (read y)) state
    -- sub X Y decreases register X by the value of Y.
    Sub x y -> 
      return $ modifyOffset (+ 1) $ modifyMemory (Map.insertWith (+) x (- read y)) state
    -- mul X Y sets register X to the result of multiplying the value contained 
    -- in register X by the value of Y.
    Mul x y -> 
      let result = read (Left x) * read y in
      return $ modifyOffset (+ 1) $ modifyMemory (Map.insert x result) state
    -- jnz X Y jumps with an offset of the value of Y, but only if the value of X is not zero. 
    -- (An offset of 2 skips the next instruction, an offset of -1 jumps to the previous 
    -- instruction, and so on.)
    Jnz x y -> 
      if read x == 0 then
        return $ modifyOffset (+ 1) state
      else 
        return $ modifyOffset (+ read y) state

run :: Seq Instr -> State -> [State]
run instrs state =
  case step instrs state of
    Nothing     -> [state]
    Just state' -> state : run instrs state'

main :: IO ()
main = do
  instrs <- parseFile parser "input/23.txt"

  let is_mul :: State -> Bool
      is_mul (State _ offset) = 
        case instrs Seq.!? offset of
          Just (Mul _ _) -> True
          _              -> False

  putStr "Part 1: "
  let start_state1 = State (Map.fromList $ map (,0) ['a'..'h']) 0
  print $ count is_mul $ run instrs start_state1 
