module Main (main) where

import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (guard)

type Keypad = Map (Char, Char) [String]

keypadRow :: [Char] -> Keypad
keypadRow row = Map.fromList $ do
  (left, right) <- zip row (tail row)
  [((left, right), pure ">"), ((right, left), pure "<")]

keypadCol :: [Char] -> Keypad
keypadCol col = Map.fromList $ do
  (top, bottom) <- zip col (tail col)
  [((top, bottom), pure "v"), ((bottom, top), pure "^")]

{- 
  +---+---+---+
  | 7 | 8 | 9 |
  +---+---+---+
  | 4 | 5 | 6 |
  +---+---+---+
  | 1 | 2 | 3 |
  +---+---+---+
      | 0 | A |
      +---+---+ 
-}
numericKeypad :: Keypad
numericKeypad = 
  transitiveClosure $ Map.unions
    [ keypadRow "789"
    , keypadRow "456"
    , keypadRow "123"
    , keypadRow "0A"  
    , keypadCol "741"
    , keypadCol "8520"
    , keypadCol "963A"
    ]

{- 
      +---+---+
      | ^ | A |
  +---+---+---+
  | < | v | > |
  +---+---+---+ 
-}
directionalKeypad :: Keypad
directionalKeypad = 
  transitiveClosure $ Map.unions
    [ keypadRow "^A"
    , keypadRow "<v>"
    , keypadCol "<"
    , keypadCol "^v"
    , keypadCol "A>"
    ]

shortest :: [String] -> [String] -> [String]
shortest paths1 paths2
  | length (head paths1) < length (head paths2) = paths1
  | length (head paths1) > length (head paths2) = paths2
  | otherwise = paths1 ++ paths2

transitiveClosure :: Keypad -> Keypad
transitiveClosure keypad = 
  let
    transitive_paths :: Keypad
    transitive_paths = Map.fromListWith shortest $ do
      ((src , mid), src_to_mids) <- Map.toList keypad
      ((mid', trg), mid_to_trgs) <- Map.toList keypad
      if src == trg then
        return ((src, src), pure "")
      else do
        guard $ mid' == mid
        src_to_mid <- src_to_mids
        mid_to_trg <- mid_to_trgs
        return ((src, trg), [src_to_mid ++ mid_to_trg])

    new_keypad :: Keypad
    new_keypad = Map.unionWith shortest keypad transitive_paths
  in
    if keypad == new_keypad then
      keypad
    else
      transitiveClosure new_keypad

numericKeypadPaths :: String -> [String]
numericKeypadPaths []  = return []
numericKeypadPaths [_] = return []
numericKeypadPaths (button1 : button2 : buttons) = do
  step_path <- numericKeypad Map.! (button1, button2) 
  rest_path <- numericKeypadPaths (button2 : buttons)
  return $ step_path ++ "A" ++ rest_path

-- directionalKeypadPath :: String -> String
-- directionalKeypadPath buttons = do
--   step <- zip buttons (tail buttons)
--   directionalKeypad Map.! step ++ "A"

main :: IO ()
main = do
  codes <- lines <$> readFile "input/21.txt"

  traverse_ print $ do
    code <- codes
    let leading_nums :: Int
        leading_nums = read $ init code

        shortest_seq :: [String]
        shortest_seq = numericKeypadPaths $ 'A' : code

    return (leading_nums, shortest_seq)
