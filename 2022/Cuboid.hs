module Cuboid 
  ( Cuboid(Cuboid)
  , Range(..)
  , points 
  , boundingBox 
  , intersect
  , subtract
  , subtractAll
  , reduce
  , volume
  , diameter
) where

import Data.List ( sortOn, mapAccumL, maximumBy, transpose )
import Prelude hiding ( subtract )
import Data.Function ( on )

data Range = Range
  { getLowerBound :: Int
  , getUpperBound :: Int }
  
newtype Cuboid = Cuboid { getRanges :: [Range] }

diameter :: Range -> Int
diameter (Range start end) = end - start + 1

-- | Merges overlapping ranges so all remaining ranges are disjoint.
reduce :: [Range] -> [Range]
reduce = go . sortOn getLowerBound . filter ((0 <) . diameter)
  where
    go :: [Range] -> [Range]
    go [] = []
    go [range] = [range]
    go (Range lb1 ub1 : Range lb2 ub2 : ranges)
      | lb2 <= ub1 = go (Range lb1 (max ub1 ub2) : ranges)
      | otherwise  = Range lb1 ub1 : go (Range lb2 ub2 : ranges)

values :: Range -> [Int]
values (Range start end) = [start .. end]

points :: Cuboid -> [[Int]]
points (Cuboid ranges) = traverse values ranges
  
-- | Volume is negative if any of the Cuboid edges is negative.
volume :: Cuboid -> Int
volume (Cuboid ranges) = abs (product edges) * sign
  where
    edges = diameter <$> ranges
    sign = minimum $ signum <$> edges
    
sliceRange :: Int -> Range -> (Range, Range)
sliceRange middle (Range start end) =
  ( Range start (min middle end)
  , Range (max start (middle+1)) end)
    
slice :: Int -> Int -> Cuboid -> (Cuboid, Cuboid)
slice axis split_value (Cuboid ranges) = (lower_cuboid, upper_cuboid)
  where
    (ranges_before, axis_range : ranges_after) = splitAt axis ranges
    (lower_range, upper_range) = sliceRange split_value axis_range
    lower_cuboid =  Cuboid (ranges_before <> [lower_range] <> ranges_after)
    upper_cuboid =  Cuboid (ranges_before <> [upper_range] <> ranges_after)
    
intersectRange :: Range -> Range -> Range
intersectRange (Range start1 end1) (Range start2 end2) = 
  Range (max start1 start2) (min end1 end2)

intersect :: Cuboid -> Cuboid -> Cuboid
intersect (Cuboid ranges1) (Cuboid ranges2) = 
  Cuboid (zipWith intersectRange ranges1 ranges2)

subtract :: Cuboid -> Cuboid -> [Cuboid]
subtract base_cuboid (Cuboid subtract_ranges) = remaining_sub_cuboids
  where
    go :: Cuboid -> (Int, Range) -> (Cuboid, [Cuboid])
    go cuboid0 (axis, Range start end) = (cuboid2, [left_part, right_part])
      where
        (left_part, cuboid1)  = slice axis (start-1) cuboid0
        (cuboid2, right_part) = slice axis end cuboid1
        
    remaining_sub_cuboids :: [Cuboid]
    remaining_sub_cuboids = 
        filter ((> 0) . volume)
      $ concat
      $ snd
      $ mapAccumL go base_cuboid
      $ zip [0..] subtract_ranges
      
overlapVolume :: Cuboid -> Cuboid -> Int
overlapVolume cuboid1 cuboid2 = 
  max 0 $ volume $ intersect cuboid1 cuboid2

subtractAll :: Cuboid -> [Cuboid] -> [Cuboid]
subtractAll base_cuboid [] = [base_cuboid]
subtractAll base_cuboid cuboids = do
  -- Find the cuboid with most overlap and subtract it from base_cuboid.
  let cuboid_most_overlap = maximumBy (compare `on` overlapVolume base_cuboid) cuboids
  -- That leaves at most 2*D sub-cuboids of a D-dimentional base_cuboid, on which we recursively apply `subtractAll`.
  rem_sub_cuboid <- base_cuboid `subtract` cuboid_most_overlap
  -- Only pass rects that actually have overlap with the sub-cuboids in the recursive call.
  let cuboids_with_any_overlap = filter ((> 0) . overlapVolume rem_sub_cuboid) cuboids
  rem_sub_cuboid `subtractAll` cuboids_with_any_overlap
  
boundingRange :: [Range] -> Range
boundingRange ranges = Range 
  (minimum $ getLowerBound <$> ranges)
  (maximum $ getUpperBound <$> ranges)

boundingBox :: [Cuboid] -> Cuboid
boundingBox = Cuboid . fmap boundingRange . transpose . fmap getRanges
