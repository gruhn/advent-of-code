module Main (main) where

import Utils (withCoords)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad (guard)
import Data.Char (digitToInt)
import Data.Containers.ListUtils (nubOrd)
import Data.Maybe (maybeToList)

type Pos = (Int,Int)

type TopographicMap = Map Pos Int

parse :: String -> TopographicMap
parse = Map.map digitToInt . Map.fromList . withCoords . lines

trailsFrom :: TopographicMap -> Pos -> Int -> [[Pos]]
trailsFrom topo_map (x,y) height =
  if height == 9 then 
    return [(x,y)]
  else do
    neighbor <- [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
    neighbor_height <- maybeToList $ Map.lookup neighbor topo_map 
    guard $ neighbor_height == height + 1
    trail <- trailsFrom topo_map neighbor neighbor_height 
    return $ (x,y) : trail

main :: IO ()
main = do
  topo_map <- parse <$> readFile "input/10.txt"

  let trailheads :: [Pos]
      trailheads = [ pos | (pos, 0) <- Map.toList topo_map ]

  putStr "Part 1: "
  let score trailhead = length $ nubOrd $ map last $ trailsFrom topo_map trailhead 0
  print $ sum $ map score trailheads

  putStr "Part 2: "
  let rating trailhead = length $ nubOrd $ trailsFrom topo_map trailhead 0
  print $ sum $ map rating trailheads
