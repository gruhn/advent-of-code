module Main where
import KnotHash (knotHash)
import qualified Data.Text as Text
import Numeric (showBin)
import Data.Char (digitToInt)
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Monad (guard)
import Utils (withCoords)

padLeft :: Int -> Char -> String -> String
padLeft n c str = replicate (n - length str) c ++ str

type Point = (Int, Int)

neighbors :: Point -> Set Point
neighbors (x,y) = 
 Set.fromList [ (x+1,y), (x-1,y), (x,y+1), (x,y-1) ]

extractComponent :: Set Point -> Point -> (Set Point, Set Point)
extractComponent all_points start = go start (Set.empty, all_points)
 where
  go :: Point -> (Set Point, Set Point) -> (Set Point, Set Point)
  go point (component, rest_points) =
   if point `Set.member` rest_points then
    foldr go (Set.insert point component, Set.delete point rest_points) (neighbors point)
   else
    (component, rest_points)

connectedComponents :: Set Point -> [Set Point]
connectedComponents all_points = 
 case Set.lookupMin all_points of
  Nothing    -> []
  Just point -> component : connectedComponents rest_points
   where
    (component, rest_points) = extractComponent all_points point

main :: IO ()
main = do
 let input = "amgozmfv"

     grid :: [[Char]]
     grid = do
      idx <- [0 .. 127] :: [Int]
      return 
       $ concatMap (padLeft 4 '0' . (`showBin` "") . digitToInt) 
       $ Text.unpack
       $ knotHash
       $ input <> "-" <> Text.pack (show idx)

     points :: Set Point
     points = Set.fromList $ do 
      (point, cell) <- withCoords grid
      guard (cell == '1')
      return point

 putStr "Part 1: "
 print $ Set.size points

 putStr "Part 2: "
 print $ length $ connectedComponents points
