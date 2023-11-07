{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main where
import Data.Map (Map)
import Utils (withCoords)
import qualified Data.Map as Map
import Control.Monad (guard)
import Data.Maybe (catMaybes)
import qualified Data.List as List

data Segment = Vertical | Horizontal | Cross | Letter Char
  deriving (Eq, Show)

toSegment :: Char -> Segment
toSegment = \case
  '|' -> Vertical
  '-' -> Horizontal
  '+' -> Cross
  ltr -> Letter ltr

type Point = (Int,Int)

parse :: String -> Map Point Segment
parse str = Map.fromList $ do
  (point, char) <- withCoords $ lines str
  guard $ char /= ' '
  return (point, toSegment char)

neighborsOf :: Map Point Segment -> Point -> Segment -> [Point]
neighborsOf segments (x,y) = \case 
    Horizontal -> catMaybes [left, right]
    Vertical   -> catMaybes [above, below]
    Cross      -> catMaybes [left, right, above, below]
    (Letter _) -> catMaybes [left, right, above, below]
  where
    left = do 
      seg <- Map.lookup (x-1,y) segments
      return $ case seg of
        Vertical -> (x-2,y)
        _        -> (x-1,y)

    right = do 
      seg <- Map.lookup (x+1,y) segments
      return $ case seg of
        Vertical -> (x+2,y)
        _        -> (x+1,y)

    above = do 
      seg <- Map.lookup (x,y-1) segments
      return $ case seg of
        Horizontal -> (x,y-2)
        _          -> (x,y-1)

    below = do 
      seg <- Map.lookup (x,y+1) segments
      return $ case seg of
        Horizontal -> (x,y+2)
        _          -> (x,y+1)

buildGraph :: Map Point Segment -> Map Point [Point]
buildGraph segments = Map.mapWithKey (neighborsOf segments) segments

unfoldPath :: Map Point [Point] -> [Point]
unfoldPath graph = go start start_next
  where
    (start, [start_next]) = Map.findMin $ Map.filter ((==1) . length) graph

    go :: Point -> Point -> [Point]
    go previous current =
      case List.delete previous (graph Map.! current) of
        []     -> [previous, current]
        [next] -> previous : go current next
        _      -> error "more than two neighbors"


diff :: Point -> Point -> Int
diff (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

main :: IO ()
main = do
  input <- parse <$> readFile "input/19.txt"    

  let get_letter :: Segment -> String
      get_letter (Letter l) = [l]
      get_letter _ = ""

      path = unfoldPath $ buildGraph input

  putStr "Part 1: "
  putStrLn $ do 
    point <- path
    let seg = input Map.! point
    get_letter seg

  putStr "Part 2: "
  print $ 1 + sum (zipWith diff path (tail path))
