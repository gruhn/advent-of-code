module Main (main) where
import Utils (Parser, integer, parseFile)
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (char, newline)
import qualified Data.Set as Set
import Data.Set (Set)
import Algorithm.Search (dijkstra)
import Data.Maybe (isJust)

type Pos = (Int,Int)

parser :: Parser [Pos]
parser = pos `sepEndBy` newline
  where
    pos :: Parser Pos
    pos = (,) <$> integer <* char ',' <*> integer

neighbors :: Pos -> [Pos]
neighbors (x,y) = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]

inBounds :: Pos -> Bool
inBounds (x,y) = 0 <= x && x <= 70 && 0 <= y && y <= 70

shortestPath :: [Pos] -> Maybe Int
shortestPath obstacles =
  let 
    start_pos  = (0,0)
    target_pos = (70,70)
    step_cost _ _ = 1

    obstacle_set :: Set Pos
    obstacle_set = Set.fromList obstacles

    walkable :: Pos -> Bool
    walkable pos = inBounds pos && pos `Set.notMember` obstacle_set
  in
    fst <$> dijkstra
      (filter walkable . neighbors)
      step_cost
      (==target_pos)
      start_pos

splitMiddle :: [a] -> ([a], [a])
splitMiddle as = splitAt (length as `div` 2) as

findFirstBlocking :: [Pos] -> [Pos] -> Maybe Pos
findFirstBlocking safe_prefix unknown_suffix =
  if isJust $ shortestPath (safe_prefix ++ unknown_suffix) then
    Nothing -- path exists ==> no blocking obstacle
  else
    case splitMiddle unknown_suffix of
      ([], [])    -> error "safe_prefix not actually safe"
      ([], [pos]) -> return pos
      ([pos], []) -> return pos
      (left_half, right_half) -> 
        if isJust $ shortestPath (safe_prefix ++ left_half) then
          findFirstBlocking (safe_prefix ++ left_half) right_half
        else
          findFirstBlocking safe_prefix left_half
      
main :: IO ()
main = do
  obstacles <- parseFile parser "input/18.txt"

  putStr "Part 1: "
  print $ shortestPath $ take 1024 obstacles

  putStr "Part 2: "
  print $ findFirstBlocking [] obstacles
