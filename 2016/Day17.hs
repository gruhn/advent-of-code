module Main where

import Crypto.Hash (hashWith, MD5 (MD5))
import Data.String (fromString, IsString)
import Data.ByteString (ByteString)
import Data.Foldable (foldl', for_)
import qualified Data.List as L
import Control.Arrow (second)
import Dijkstra (dijkstra)
import Control.Monad (guard)

md5 :: String -> String
md5 = show . hashWith MD5 . to_byte_string
  where
    to_byte_string :: String -> ByteString
    to_byte_string = fromString

type Move = Char

type State = [Move]

nextStates :: String -> State -> [State]
nextStates passcode moves = next_states
  where
    doors = take 4 $ md5 (passcode <> reverse moves)

    is_open :: Char -> Bool
    is_open door = door `L.elem` "bcdef"

    directions = "UDLR"

    open_directions = fmap fst 
      $ filter (is_open . snd) 
      $ zip directions doors 

    is_in_bounds :: State -> Bool
    is_in_bounds moves =
      let (x,y) = head $ positionsFrom (0,0) moves
      in  0 <= x && x <= 3 && 0 <= y && y <= 3

    next_states = do
      move <- open_directions
      let state = move : moves
      guard (is_in_bounds state)
      return state

positionsFrom :: (Int,Int) -> [Move] -> [(Int,Int)]
positionsFrom = L.scanr go
  where
    go :: Move -> (Int,Int) -> (Int,Int)
    go move (x,y) =
      case move of
        'U' -> (x,y-1); 'D' -> (x,y+1)
        'R' -> (x+1,y); 'L' -> (x-1,y)
        _ -> error "undefined move"

isFinalState :: State -> Bool
isFinalState moves = ends_in_vault && was_not_at_vault_before
  where
    positions = positionsFrom (0,0) moves
    ends_in_vault = head positions == (3,3)
    was_not_at_vault_before = (3,3) `L.notElem` tail positions

main :: IO ()
main = do
  let passcode = "qljzarfv"
      transition_cost _ _ = 1
      start_state = ""

      paths_to_vault = filter (isFinalState . fst) 
        $ dijkstra (nextStates passcode) transition_cost start_state

  putStr "Part 1: "
  print $ head paths_to_vault

  putStr "Part 2: "
  for_ (snd <$> paths_to_vault) print