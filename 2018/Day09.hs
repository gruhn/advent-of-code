module Main where
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map as Map

data State = State 
  { nextMarble         :: Int
  , currentMarbleIndex :: Int
  , marbleRing         :: Seq Int
  } deriving Show

insertMarble :: State -> State
insertMarble (State new_marble current items) = 
 if new_marble `mod` 23 == 0 then
  let
    delete_index = (current - 7) `mod` length items 
    new_items    = Seq.deleteAt delete_index items
  in
    State (new_marble+1) ((delete_index+1) `mod` length items) new_items
else
  let 
    new_current = (current + 2) `mod` length items
    new_items = Seq.insertAt new_current new_marble items
  in 
    State (new_marble+1) new_current new_items

play :: [State]
play = iterate insertMarble $ State 1 0 (Seq.fromList [0])

drop23 :: [a] -> [a]
drop23 as = 
  case drop 23 as of
    [] -> []
    (a:as') -> a : drop23 as'

winningPlayers :: Int -> [(Int, Int)]
winningPlayers player_count = [ ((23*i-1) `mod` player_count, i*23) | i <- [1..] ] 

highScore :: Int -> Int -> Int
highScore player_count last_marble_worth =
  maximum $ Map.elems $ go player_count last_marble_worth

go :: Int -> Int -> Map Int Int
go player_count last_marble_worth =
  play 
    & takeWhile ((<= last_marble_worth) . nextMarble)
    & drop23
    & zip (winningPlayers player_count)
    & map score
    & Map.fromListWith (+)

score :: ((Int, Int), State) -> (Int, Int)
score ((p, s), State _ current items) = 
  let 
    delete_index = (current - 7) `mod` length items 
    delete_item  = items `Seq.index` delete_index
  in
    (p, s + delete_item)

main :: IO ()
main = do
  putStrLn "TESTS: "
  print $ highScore 10 1618 == 8317
  print $ highScore 13 7999 == 146373
  print $ highScore 17 1104 == 2764
  print $ highScore 21 6111 == 54718
  print $ highScore 30 5807 == 37305  

  putStr "Part 1: "
  print $ highScore 439 71307 

  putStr "Part 2: "
