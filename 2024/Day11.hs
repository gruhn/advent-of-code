module Main (main) where

import Control.Monad.Trans.State (State)
import qualified Control.Monad.Trans.State as State
import Control.Monad (guard)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap

splitStone :: Int -> Maybe (Int, Int)
splitStone stone = do
  let stone_str = show stone
  let digit_count = length stone_str
  guard $ even digit_count
  let (left, right) = splitAt (digit_count `div` 2) stone_str
  return (read left, read right)

type Cache = IntMap (IntMap Int)

insertCache :: Int -> Int -> Int -> Cache -> Cache
insertCache step stone result = IntMap.insertWith (<>) step (IntMap.singleton stone result)

lookupCache :: Int -> Int -> Cache -> Maybe Int
lookupCache step stone cache = IntMap.lookup step cache >>= IntMap.lookup stone

countMem :: Int -> Int -> State Cache Int
countMem step stone = do
  maybe_result <- State.gets (lookupCache step stone)
  case maybe_result of
    Just result -> return result
    Nothing -> do
      result <- count step stone
      State.modify (insertCache step stone result)
      return result

count :: Int -> Int -> State Cache Int
count 0    _     = return 1
count step 0     = countMem (step-1) 1
count step stone =
  case splitStone stone of
    Nothing -> countMem (step-1) (stone*2024)
    Just (stone_left, stone_right) -> do
      left_count  <- countMem (step-1) stone_left  
      right_count <- countMem (step-1) stone_right 
      return (left_count + right_count)

countAll :: Int -> [Int] -> Int
countAll steps stones = 
  sum $ State.evalState (traverse (countMem steps) stones) IntMap.empty

parse :: String -> [Int]
parse = map read . words

main :: IO ()
main = do
  stones <- parse <$> readFile "input/11.txt"
      
  putStr "Part 1: "
  print $ countAll 25 stones

  putStr "Part 2: "
  print $ countAll 75 stones
