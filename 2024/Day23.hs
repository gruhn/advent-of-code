module Main (main) where
import Utils (Parser, parseFile, select)
import Text.Megaparsec (some, sepEndBy)
import Text.Megaparsec.Char (lowerChar, string, newline)
import Data.Containers.ListUtils (nubOrd)
import Data.Tuple (swap)
import Control.Monad (guard)
import Data.Map.Strict (Map)
import Data.List (tails, sort, isPrefixOf, sortOn, intercalate)
import qualified Data.Map as Map
import Data.Foldable (asum, traverse_, for_)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.List (find)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Foldable (maximumBy)
import Data.Ord (comparing, Down (Down))
import Debug.Trace

parser :: Parser [(String, String)]
parser = line `sepEndBy` newline
  where
    line :: Parser (String, String)
    line = do
      from <- some lowerChar
      string "-"
      to  <- some lowerChar
      return (from, to)

-- step :: [(String, String)] -> [[String]] -> [[String]]
-- step edges groups = fromMaybe groups $ asum $ do
--   (g, gs) : rest_groups <- tails $ Map.toList groups
--   case find (\(g2, gs) -> (g, g2) `elem` edges) rest_groups of
--     Nothing -> return Nothing
--     Just (g2, gs2) ->
--       return $ Just $ Map.insertWith (++) g gs2 $ Map.delete g2 groups

insert :: [(String, String)] -> String -> [[String]] -> [[String]]
insert edges item [] = [[item]]
insert edges item (group : groups) =
  if all (\it -> (item, it) `elem` edges) group then
    (item:group) : insert edges item groups
  else
    group : insert edges item groups

subsets :: [a] -> [[a]]
subsets []     = [[]]
subsets (a:as) = map (a:) (subsets as) ++ subsets as

buckets :: Set (String, String) -> [String] -> [[String]]
buckets edges [] = []
buckets edges nodes = nubOrd $ do
  node1 : rest1 <- tails nodes
  bucket <- [] : buckets edges rest1
  guard $ and $ do
    node2 <- bucket
    return $ (node1, node2) `Set.member` edges
  return $ sort (node1:bucket)

allConnected :: Map String (Set String) -> Set String -> Bool
allConnected edges str_set = and $ do
  node <- Set.toList str_set
  return $ Set.isSubsetOf str_set $ Set.insert node (edges Map.! node)

go :: Map String (Set String) -> [Set String]
go edges = do
  (node, neighbors) <- Map.toList edges
  (_, rest) <- select $ Set.toList neighbors
  let rest_set = Set.fromList rest
  guard $ allConnected edges rest_set
  return $ Set.insert node rest_set

main :: IO ()
main = do
  input <- parseFile parser "input/23.txt"

  let comps = nubOrd $ concat $ [ [from, to] | (from, to) <- input ]
      input' = Set.fromList $ map swap input ++ input

      edges :: Map String (Set String)
      edges = Map.fromListWith (<>) $ do
        (from, to) <- input
        [ (from, Set.singleton to), (to, Set.singleton from) ]

  for_ (map Set.toAscList $ go edges) $ \group -> do -- Map.unionWith (<>) edges $ Map.fromList [ (node, Set.singleton node) | node <- comps ]
    putStrLn $ intercalate "," group
