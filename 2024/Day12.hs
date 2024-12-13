module Main (main) where
import Utils (withCoords, countBy)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad (guard)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (maybeToList)

type Pos = (Int,Int)

neighbors :: Pos -> [Pos]
neighbors (x,y) = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]

type Region = Set Pos

-- | perimeter = number of neighbors right outside the region
perimeter :: Region -> Int
perimeter region = length $ do
  pos <- Set.toList region
  neighbor <- neighbors pos
  guard $ neighbor `notElem` region
  return neighbor

-- | area = number of points in the region
area :: Region -> Int
area = Set.size

-- | All immediate neighbors outside the region.
surface :: Region -> Region
surface region = Set.fromList (concatMap neighbors (Set.toList region)) Set.\\ region

-- | Number of times `(x,y)` forms a corner with the region.
cornersAt :: Region -> Pos -> Int
cornersAt region (x,y) = countBy is_corner diagonal_dirs
  where
    diagonal_dirs :: [Pos]
    diagonal_dirs = [(-1,-1), (-1,1), (1,-1), (1,1)]

    is_corner :: Pos -> Bool
    is_corner (dx, dy) =
      let
        diagonal_pos    = (x+dx, y+dy)
        shared_neigbor1 = (x+dx, y)
        shared_neigbor2 = (x, y+dy)

        is_convex_corner = 
          (shared_neigbor1 `notElem` region) && 
          (shared_neigbor2 `notElem` region)

        is_concave_corner = 
          (shared_neigbor1 `elem` region) &&
          (shared_neigbor2 `elem` region) &&
          (diagonal_pos `notElem` region)
      in
        is_convex_corner || is_concave_corner

-- | sides == number of corners of the region.
sides :: Region -> Int
sides region = sum $ cornersAt region <$> Set.toList region

-- | Partitions a set of points into a connected component and
-- the remaining points.
extractRegion :: Pos -> Set Pos -> (Region, Set Pos)
extractRegion pos = expand (Set.singleton pos)
  where
    expand :: Set Pos -> Set Pos -> (Set Pos, Set Pos)
    expand region rest_poses =
      let new_poses = surface region `Set.intersection` rest_poses in
      if null new_poses then
        (region, rest_poses)
      else
        expand
          (region <> new_poses)
          (rest_poses Set.\\ new_poses)

-- | Partitions a set of points into all connected components.
connectedRegions :: Set Pos -> [Region]
connectedRegions poses0 = do
  (pos, poses1) <- maybeToList $ Set.maxView poses0
  let (region, poses2) = extractRegion pos poses1
  region : connectedRegions poses2

main :: IO ()
main = do
  pos_char_pairs <- withCoords . lines <$> readFile "input/12.txt"

  let grouped_by_char :: Map Char (Set Pos)
      grouped_by_char = Map.fromListWith (<>) $ do
        (pos, char) <- pos_char_pairs
        return (char, Set.singleton pos)

      regions :: [Region]
      regions = concatMap connectedRegions $ Map.elems grouped_by_char

  putStr "Part 1: "
  print $ sum [ perimeter region * area region | region <- regions ]

  putStr "Part 2: "
  print $ sum [ sides region * area region | region <- regions ]
