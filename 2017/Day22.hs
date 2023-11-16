module Main where
import Utils (withCoords, Vec2(..), rotateLeft90, rotateRight90, count)
import Control.Monad (guard)
import Data.Map (Map)
import qualified Data.Map as Map

-- clean nodes are represented by not storing them
data Node = Weakened | Infected | Flagged
  deriving Show

data State = State 
  { getPos   :: Vec2 Int
  , getDir   :: Vec2 Int
  , getNodes :: Map (Vec2 Int) Node
  } deriving Show

step1 :: State -> State 
step1 (State pos dir nodes) = 
  case Map.lookup pos nodes of
    Nothing -> -- clean
      let new_dir = rotateLeft90 dir in
      State (pos+new_dir) new_dir (Map.insert pos Infected nodes)
    Just Infected -> 
      let new_dir = rotateRight90 dir in
      State (pos+new_dir) new_dir (Map.delete pos nodes)
    _otherwise -> 
      error "other weakened/flagged nodes not supported in part 1"

step2 :: State -> State
step2 (State pos dir nodes) =
  case Map.lookup pos nodes of
    Nothing -> -- clean
      let new_dir = rotateLeft90 dir in
      State (pos+new_dir) new_dir (Map.insert pos Weakened nodes)
    Just Weakened -> 
      State (pos+dir) dir (Map.insert pos Infected nodes)
    Just Infected -> 
      let new_dir = rotateRight90 dir in
      State (pos+new_dir) new_dir (Map.insert pos Flagged nodes)
    Just Flagged -> 
      let new_dir = negate dir in
      State (pos+new_dir) new_dir (Map.delete pos nodes)

isInfectionBurst :: State -> Bool
isInfectionBurst (State pos dir nodes) =
  let prev_pos = pos-dir in
  case Map.lookup prev_pos nodes of
    Just Infected -> True
    _otherwise    -> False

main :: IO ()
main = do 
  input <- lines <$> readFile "input/22.txt"

  let infected_nodes :: Map (Vec2 Int) Node
      infected_nodes = Map.fromList $ do
        ((x,y), char) <- withCoords input
        guard $ char == '#'
        return (Vec2 x y, Infected)

      middle :: Int
      middle = length (head input) `div` 2

      start_state :: State
      start_state = State 
        { getPos = Vec2 middle middle
        , getDir = Vec2 0 (-1)
        , getNodes = infected_nodes
        }

  putStr "Part 1: "
  print
    $ count isInfectionBurst
    $ tail -- don't cout start state
    $ take (10000+1)
    $ iterate step1 start_state

  putStr "Part 2: "
  print 
    $ count isInfectionBurst 
    $ tail -- don't cout start state
    $ take (10000000+1)
    $ iterate step2 start_state
