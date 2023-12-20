module Main where

import Utils (Parser, parseFile, iterateJust, takeUntil)
import Text.Megaparsec (sepEndBy, sepBy, some, choice)
import Text.Megaparsec.Char (newline, string, char, lowerChar)
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace
import Data.Foldable (traverse_)
import qualified Data.List as List

data GateType = FlipFlop | Broadcaster | Conjunction
  deriving Show

data Gate = Gate GateType [String]
  deriving Show

parser :: Parser (Map String Gate)
parser = Map.fromList <$> gate `sepEndBy` newline
  where
    gate :: Parser (String, Gate)
    gate = do
      gtype <- gate_type
      name <- gate_name
      string " -> "
      targets <- gate_name `sepBy` string ", "
      return (name, Gate gtype targets)

    gate_name :: Parser String
    gate_name = some lowerChar

    gate_type :: Parser GateType
    gate_type = choice
      [ FlipFlop    <$ char '%'
      , Conjunction <$ char '&'
      , Broadcaster <$ string ""
      ]

type GateInputs = Map String (Map String Bool)

type GateStates = Map String Bool

data Action = Action 
  { getSource :: String
  , getPulse  :: Bool
  , getTarget :: String
  } deriving Show

data State = State
  { getGateQueue  :: [Action]
  , getGateInputs :: GateInputs
  , getGateValue  :: GateStates
  } deriving Show

updateGateInput :: String -> Bool -> String -> GateInputs -> GateInputs
updateGateInput source value gate =
  Map.insertWith Map.union gate (Map.singleton source value)

updateGateInputs :: String -> Bool -> [String] -> GateInputs -> GateInputs
updateGateInputs source value gates input_map =
  foldr (updateGateInput source value) input_map gates

initialize :: Map String Gate -> State
initialize gates =
  let initial_inputs :: GateInputs
      initial_inputs =
          updateGateInput "button" False "broadcaster"
        $ Map.foldrWithKey go Map.empty gates
        where
          go :: String -> Gate -> GateInputs -> GateInputs
          go source (Gate _ targets) = updateGateInputs source False targets

   in State [] initial_inputs Map.empty

step :: Map String Gate -> State -> State
step _gates (State [] gate_inputs gate_states) = 
  State [Action "button" False "broadcaster"] gate_inputs gate_states
step gates (State (Action source pulse gate : queue) gate_inputs gate_states) =  
  case Map.lookup gate gates of
    Nothing ->
      State queue gate_inputs gate_states

    Just (Gate Broadcaster targets) ->
      let output = gate_inputs Map.! gate Map.! source in
      State (queue ++ map (Action gate output) targets)
            gate_inputs
            gate_states

    Just (Gate FlipFlop targets) ->
      if pulse then
        State queue gate_inputs gate_states
      else
        let output = not $ Map.findWithDefault False gate gate_states in
        State (queue ++ map (Action gate output) targets)
              gate_inputs
              (Map.insert gate output gate_states)

    Just (Gate Conjunction targets) ->
      let gate_inputs_updated = updateGateInput source pulse gate gate_inputs 
          output = not $ and $ gate_inputs_updated Map.! gate 
      in
      State (queue ++ map (Action gate output) targets)
            gate_inputs_updated            
            gate_states

takeButtonPresses :: Int -> [Action] -> [Action]
takeButtonPresses n [] = undefined
takeButtonPresses n (action:actions) =
  if getSource action == "button" then
    if n > 0 then
      action : takeButtonPresses (n-1) actions
    else
      []
  else
    action : takeButtonPresses n actions

countPulses :: [Action] -> (Int, Int)
countPulses actions = (length high_pulses, length low_pulses)
  where (high_pulses, low_pulses) = List.partition getPulse actions

main :: IO ()
main = do
  gates <- parseFile parser "input/20.txt"

  putStr "Part 1: "
  print 
    $ uncurry (*)
    $ countPulses
    $ takeButtonPresses 1000
    $ map head
    $ filter (not . null)
    $ map getGateQueue
    $ iterate (step gates) 
    $ initialize gates

  let cycle_length gt = 
          length
        $ filter ((=="button") . getSource)
        $ map head
        $ filter (not . null)
        $ map getGateQueue
        $ takeUntil (\case (State (Action src pls trg:_) gate_inputs _) -> trg == "mg" && (gate_inputs Map.! "mg" Map.! gt); _ -> False)
        $ iterate (step gates) 
        $ initialize gates

  putStr "Part 2: "
  print $ foldr lcm 1 $ map cycle_length ["hf","jg", "jm", "rh"]
