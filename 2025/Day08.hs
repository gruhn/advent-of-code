module Main (main) where
import Utils (Parser, parseFile, Vec3 (..), takeUntil)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec (sepEndBy)
import Data.List (tails, sort, group, scanl', sortOn, find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (Down(..))
import Data.Foldable (traverse_)
import Data.Foldable (Foldable(toList))

type Pos = Vec3 Int

parser :: Parser [Pos]
parser = pos `sepEndBy` newline
  where
    pos = do
      x <- decimal
      char ','
      y <- decimal
      char ','
      z <- decimal
      return $ Vec3 x y z

dist :: (Pos, Pos) -> Double
dist (v, w) = sum $ do
  (a, b) <- zip (toList v) (toList w)
  return $ (fromIntegral a - fromIntegral b)^2

closestPairs :: [Pos] -> [(Pos, Pos)]
closestPairs poses = sortOn dist (do
  (posA:rest_poses) <- tails poses
  posB <- rest_poses
  return (posA, posB))

type Clustering = Map Pos Int

joinClusters :: Clustering -> (Pos, Pos) -> Clustering
joinClusters clusters (pos_A, pos_B) =
  let
    id_A = clusters Map.! pos_A
    id_B = clusters Map.! pos_B
  in
    if id_A == id_B then
      clusters
    else
      Map.map (\id -> if id == id_B then id_A else id) clusters

sizes :: Clustering -> [Int]
sizes = map length . group . sort . Map.elems

main :: IO ()
main = do
  poses <- parseFile parser "input/08.txt"

  let initial_clusters = Map.fromList $ zip poses [0..]
  let closest_pairs = closestPairs poses
  let cluster_size_history = map sizes $ scanl' joinClusters initial_clusters closest_pairs

  putStr "Part 1: "
  print
    $ product
    $ take 3
    $ sortOn Down
    $ cluster_size_history !! 1000

  putStr "Part 2: "
  let is_one_cluster (_, cluster_sizes) = length cluster_sizes == 1
  print $ do
    ((Vec3 x1 _ _, Vec3 x2 _ _), _) <- find is_one_cluster $ zip closest_pairs (tail cluster_size_history)
    return (x1 * x2)
