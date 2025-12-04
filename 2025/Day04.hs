module Main (main) where
import Utils (withCoords, converge)
import Control.Monad (guard)
import Data.Set (Set)
import qualified Data.Set as Set

type Pos = (Int,Int)

parse :: String -> Set Pos
parse input = Set.fromList $ do
  (pos, '@') <- withCoords $ lines input
  return pos

neighbors :: Set Pos -> Pos -> Set Pos
neighbors grid (x,y) = Set.fromList $ do
  dx <- [-1,0,1]
  dy <- [-1,0,1]
  let neighbor = (x+dx, y+dy)
  guard $ neighbor /= (x,y)
  guard $ Set.member neighbor grid
  return neighbor

isRemovable :: Set Pos -> Pos -> Bool
isRemovable grid pos = Set.size (neighbors grid pos) < 4

removeRemovable :: Set Pos -> Set Pos
removeRemovable grid = Set.filter (not . isRemovable grid) grid

main :: IO ()
main = do
  grid <- parse <$> readFile "input/04.txt"

  putStr "Part 1: "
  print $ Set.size grid - Set.size (removeRemovable grid)

  putStr "Part 2: "
  print $ Set.size grid - Set.size (converge removeRemovable grid)
