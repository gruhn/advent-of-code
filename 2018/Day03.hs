module Main where
import ParseUtils ( Parser, parseWith )
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (string, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (tails, foldl')
import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (guard)

type ClaimID = Int

data Claim = Claim
  { getID     :: ClaimID
  , getX      :: Int
  , getY      :: Int
  , getWidth  :: Int
  , getHeight :: Int
  } deriving Show

parser :: Parser [Claim]
parser = 
  let
    claim :: Parser Claim
    claim = Claim 
      <$  string "#"
      <*> decimal <* string " @ "
      <*> decimal <* string ","
      <*> decimal <* string ": "
      <*> decimal <* string "x"
      <*> decimal
  in
    claim `sepEndBy` newline

mkPositionMap :: [Claim] -> Map (Int,Int) (Set ClaimID)
mkPositionMap claims = Map.fromListWith Set.union $ do
  Claim claim_id x y width height <- claims
  dx <- [0 .. width-1]
  dy <- [0 .. height-1]
  return ((x+dx, y+dy), Set.singleton claim_id)

main :: IO ()
main = do
  claims <- parseWith parser "input/03.txt"
  let positions = mkPositionMap claims

  putStr "Part 1: "
  let positions_with_overlap = Map.filter ((>1) . Set.size) positions
  print $ Map.size positions_with_overlap

  putStr "Part 2: "
  let claims_with_overlap = Set.unions $ Map.elems positions_with_overlap
  print $ do
    Claim claim_id _ _ _ _ <- claims
    guard $ Set.notMember claim_id claims_with_overlap
    return claim_id

