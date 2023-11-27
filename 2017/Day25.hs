module Main where
import Data.Map (Map)
import qualified Data.Map as Map
import Utils (Parser, parseFile)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char (upperChar, space1)
import Text.Megaparsec.Char.Lexer (decimal)
import Control.Applicative (empty, Alternative (many, (<|>)))
import Data.Text (Text)

type State = Char

data Direction = Lft | Rgt
  deriving Show

data Transition = Trans 
  { getWriteValue  :: Int
  , getDirection   :: Direction 
  , getTargetState :: State 
  } deriving Show

data TuringMachine = TM 
  { getStartState  :: State
  , getTransitions :: Map (State, Int) Transition 
  } deriving Show

parser :: Parser (Int, TuringMachine)
parser = turing_machine
  where
    space :: Parser ()
    space = L.space space1 empty empty

    lexeme :: Parser a -> Parser a
    lexeme = L.lexeme space

    symbol :: Text -> Parser Text
    symbol = L.symbol space

    state :: Parser State
    state = upperChar

    turing_machine :: Parser (Int, TuringMachine)
    turing_machine = do
      space
      start_state <- symbol "Begin in state" *> state <* symbol "."
      symbol "Perform a diagnostic checksum after" 
      run_steps <- lexeme decimal <* symbol "steps."
      transitions <- many transition_rule
      return (run_steps, TM start_state (Map.unions transitions))

    transition_rule :: Parser (Map (State, Int) Transition)
    transition_rule = do
      source_state <- symbol "In state" *> state <* symbol ":"
      symbol "If the current value is 0:"
      transition0 <- transition
      symbol "If the current value is 1:"
      transition1 <- transition
      return $ Map.fromList 
        [ ((source_state, 0), transition0)
        , ((source_state, 1), transition1)
        ]

    direction :: Parser Direction
    direction = (Lft <$ symbol "left") <|> (Rgt <$ symbol "right")

    transition :: Parser Transition
    transition = Trans
      <$ symbol "- Write the value"      <*> decimal   <* symbol "."
      <* symbol "- Move one slot to the" <*> direction <* symbol "."
      <* symbol "- Continue with state"  <*> state     <* symbol "."

type Tape = ([Int], Int, [Int])

initialTape :: Tape
initialTape = ([], 0, [])

move :: Direction -> Tape -> Tape
move Lft ([]  , c,   rs) = ([]  , 0, c:rs)
move Lft (l:ls, c,   rs) = (ls  , l, c:rs)
move Rgt (ls  , c,   []) = (c:ls, 0, [])
move Rgt (ls  , c, r:rs) = (c:ls, r, rs)

writeValue :: Int -> Tape -> Tape
writeValue val (ls, _, rs) = (ls, val, rs)

readValue :: Tape -> Int
readValue (_, val, _) = val

simulate :: TuringMachine -> [(State, Tape)]
simulate (TM start_state transitions) = 
  let step :: (State, Tape) -> (State, Tape)
      step (source_state, tape) = 
        case Map.lookup (source_state, readValue tape) transitions of
          Nothing -> error "no defined transition"
          Just (Trans new_value dir target_state) -> 
            (target_state, move dir $ writeValue new_value tape)

   in iterate step (start_state, initialTape)

checksum :: Tape -> Int
checksum (ls, val, rs) = sum ls + val + sum rs

main :: IO ()
main = do
  (run_steps, turing_machine) <- parseFile parser "input/25.txt"

  putStr "Part 1: "
  print $ checksum . snd $ simulate turing_machine !! run_steps
