module Main (main) where
import Utils (withCoords)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad (guard)
import Data.Char (digitToInt)
import Data.Containers.ListUtils (nubOrd)

type Pos = (Int,Int)

type Grid = Map Pos Int

neighbors :: Grid -> Pos -> [Pos]
neighbors grid (x,y) = filter (`Map.member` grid) [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]

reachable :: Grid -> Pos -> Int -> [[Pos]]
reachable grid pos height = do
  neigh <- neighbors grid pos
  case Map.lookup neigh grid of
    Nothing -> []
    Just h  -> do
      guard $ h == height + 1
      if h == 9 then
        return [neigh]
      else do
        path <- reachable grid neigh h
        return $ neigh : path

main :: IO ()
main = do
  input <- Map.map digitToInt . Map.fromList . withCoords . lines <$> readFile "input/10.txt"

  putStr "Part 1: "
  print $ sum $ do
    (pos, h) <- Map.toList input
    guard $ h == 0
    return $ length $ nubOrd $ map last $ reachable input pos 0

  putStr "Part 2: "
  print $ sum $ do
    (pos, h) <- Map.toList input
    guard $ h == 0
    return $ length $ nubOrd $ reachable input pos 0
