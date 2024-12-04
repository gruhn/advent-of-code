module Main (main) where

import Utils (withCoords, takeWhileJust)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad (guard)
import Data.List (isPrefixOf)
import Data.Maybe (maybeToList)

type Grid = Map (Int,Int) Char

data Dir
  = NorthWest
  | North
  | NorthEast
  | West
  | East
  | SouthWest
  | South
  | SouthEast

step :: Dir -> (Int,Int) -> (Int,Int)
step dir (x,y) =
  case dir of
    NorthWest -> (x-1, y-1)
    North     -> (x  , y-1)
    NorthEast -> (x+1, y-1)
    West      -> (x-1, y  )
    East      -> (x+1, y  )
    SouthWest -> (x-1, y+1)
    South     -> (x  , y+1)
    SouthEast -> (x+1, y+1)

pathChars :: Grid -> Dir -> (Int,Int) -> String
pathChars grid dir start_pos = takeWhileJust $ do
  pos <- iterate (step dir) start_pos
  return $ Map.lookup pos grid

crossChars :: Grid -> (Int,Int) -> Maybe (String, String)
crossChars grid mid_pos = do
  mid_char <- Map.lookup mid_pos grid
  nw_char  <- Map.lookup (step NorthWest mid_pos) grid
  ne_char  <- Map.lookup (step NorthEast mid_pos) grid
  sw_char  <- Map.lookup (step SouthWest mid_pos) grid
  se_char  <- Map.lookup (step SouthEast mid_pos) grid
  return 
    ( [nw_char, mid_char, se_char]
    , [ne_char, mid_char, sw_char]
    )

main :: IO ()
main = do
  input <- readFile "input/04.txt"
  let grid = Map.fromList $ withCoords $ lines input

  putStr "Part 1: "
  print $ length $ do 
    start_pos <- Map.keys grid
    dir <- [ NorthWest, North, NorthEast, West, East, SouthWest, South, SouthEast ]
    guard $ "XMAS" `isPrefixOf` pathChars grid dir start_pos

  putStr "Part 2: "
  print $ length $ do
    mid_pos <- Map.keys grid
    (nw_to_se, ne_to_sw) <- maybeToList $ crossChars grid mid_pos
    guard $ nw_to_se == "MAS" || nw_to_se == reverse "MAS"
    guard $ ne_to_sw == "MAS" || ne_to_sw == reverse "MAS"
