module Main where
import Utils (Parser, parseFile, integer, lexeme, symbol)
import Text.Megaparsec.Char (newline, lowerChar)
import Text.Megaparsec (choice, (<|>), sepEndBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Prelude hiding (read)
import Control.Monad (guard)

type Value = Either Char Int

data Instr =
    Snd Value
  | Set Char Value
  | Add Char Value
  | Mul Char Value
  | Mod Char Value
  | Rcv Char
  | Jgz Value Value
  deriving Show

parser :: Parser (Seq Instr)
parser = Seq.fromList <$> instr `sepEndBy` newline
  where
    value :: Parser (Either Char Int)
    value = (Left <$> lexeme lowerChar) <|> (Right <$> integer)

    instr :: Parser Instr
    instr = choice 
      [ Snd <$ symbol "snd" <*> value
      , Set <$ symbol "set" <*> lexeme lowerChar <*> value
      , Add <$ symbol "add" <*> lexeme lowerChar <*> value
      , Mul <$ symbol "mul" <*> lexeme lowerChar <*> value
      , Mod <$ symbol "mod" <*> lexeme lowerChar <*> value
      , Rcv <$ symbol "rcv" <*> lexeme lowerChar
      , Jgz <$ symbol "jgz" <*> value <*> value
      ]

data State = State 
  { getProgramID :: Int
  , getMemory :: Map Char Int 
  , getOffset :: Int
  , getInput :: [Int]
  , getOutput :: [Int]
  } deriving (Show, Eq)

popInput :: State -> Maybe (Int, State)
popInput state = 
  case getInput state of
    []     -> Nothing 
    (x:xs) -> Just (x, state { getInput = xs })

pushOutput :: Int -> State -> State
pushOutput x state = state { getOutput = x : getOutput state }

modifyMemory :: (Map Char Int -> Map Char Int) -> State -> State
modifyMemory  f state = state { getMemory = f (getMemory state) }

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
    -- snd X sends the value of X to the other program. These values wait in a queue until 
    -- that program is ready to receive them. Each program has its own message queue, so a 
    -- program can never receive a message it sent.
    Snd x -> 
      return $ modifyOffset (+ 1) $ pushOutput (read x) state
    -- set X Y sets register X to the value of Y.
    Set x y -> 
      return $ modifyOffset (+ 1) $ modifyMemory (Map.insert x (read y)) state
    -- add X Y increases register X by the value of Y.
    Add x y -> 
      return $ modifyOffset (+ 1) $ modifyMemory (Map.insertWith (+) x (read y)) state
    -- mul X Y sets register X to the result of multiplying the value contained 
    -- in register X by the value of Y.
    Mul x y -> 
      let result = read (Left x) * read y in
      return $ modifyOffset (+ 1) $ modifyMemory (Map.insert x result) state
    -- mod X Y sets register X to the remainder of dividing the value contained 
    -- in register X by the value of Y (that is, it sets X to the result of X modulo Y).
    Mod x y -> 
      let result = read (Left x) `rem` read y in
      return $ modifyOffset (+ 1) $ modifyMemory (Map.insert x result) state
    -- rcv X receives the next value and stores it in register X. If no values are in the 
    -- queue, the program waits for a value to be sent to it. Programs do not continue to 
    -- the next instruction until they have received a value. Values are received in the 
    -- order they are sent. 
    Rcv x -> do
      (input, state') <- popInput state
      return $ modifyOffset (+ 1) $ modifyMemory (Map.insert x input) state'
    -- jgz X Y jumps with an offset of the value of Y, but only if the value of X is greater 
    -- than zero. (An offset of 2 skips the next instruction, an offset of -1 jumps to the 
    -- previous instruction, and so on.)
    Jgz x y -> 
      if read x > 0 then
        return $ modifyOffset (+ read y) state
      else 
        return $ modifyOffset (+ 1) state

run :: Seq Instr -> State -> State
run instrs state =
  case step instrs state of
    Nothing     -> state
    Just state' -> run instrs state'

startState :: Int -> State
startState program_id = State program_id (Map.singleton 'p' program_id) 0 [] []

transmitMessages :: State -> State -> (State, State)
transmitMessages p0 p1 = (p0_ready, p1_ready)
  where
    p0_messages = reverse (getOutput p0)
    p1_messages = reverse (getOutput p1)

    p0_ready = p0 { getOutput = [], getInput = getInput p0 ++ p1_messages }
    p1_ready = p1 { getOutput = [], getInput = getInput p1 ++ p0_messages }

schedule :: Seq Instr -> State -> State -> [State]
schedule instrs p0 p1
  | p0 == p0_stuck && p1 == p1_stuck = []
  | otherwise = p0_stuck : p1_stuck : schedule instrs p0_ready p1_ready
  where
    p0_stuck = run instrs p0
    p1_stuck = run instrs p1

    (p0_ready, p1_ready) = transmitMessages p0_stuck p1_stuck
  
main :: IO ()
main = do
  instrs <- parseFile parser "input/18.txt"

  putStr "Part 1: "
  print $ head $ getOutput $ run instrs (startState 0)

  putStr "Part 2: "
  print $ length $ do
    state <- schedule instrs (startState 0) (startState 1)
    guard (getProgramID state == 1)
    getOutput state
