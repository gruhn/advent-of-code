module Main where
import Utils (Parser, parseHardError, takeUntil, converge)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (newline, string, char)
import Text.Megaparsec (sepBy)
import qualified Data.Set as S
import Data.Set (Set)
import Data.List (uncons, unfoldr)
import Data.Foldable (maximumBy)
import Data.Foldable (find)
import Data.Maybe (isJust)

type Point = (Int,Int)
type Path = [Point]

parser :: Parser [Path]
parser = path `sepBy` newline
  where
    path = point `sepBy` string " -> "
    point = (,) <$> decimal <* char ',' <*> decimal

expandPath :: Path -> Path
expandPath [] = [] 
expandPath [point] = [point]
expandPath ((x1,y1):(x2,y2):points) = segment_points <> rest_points
  where
    dx = signum (x1 - x2)
    dy = signum (y1 - y2)
    add_shift (x,y) = (x-dx, y-dy)

    segment_points = takeWhile (/=(x2,y2)) $ iterate add_shift (x1,y1) 
    rest_points = expandPath ((x2,y2):points)
    
pathPoints :: [Path] -> Set Point
pathPoints paths = S.unions (S.fromList . expandPath <$> paths)

step :: (Point -> Bool) -> Point -> Maybe (Point, Point)
step is_blocked (x,y) = do
  (next, _) <- uncons $ filter (not . is_blocked) [(x,y+1),(x-1,y+1),(x+1,y+1)]
  return (next, next)

abyssLevel :: Set Point -> Int
abyssLevel rock = 1 + maximum (S.map snd rock)

spawnSand :: Point -> Set Point -> Set Point -> Set Point
spawnSand entry rock sand = case sand_path of 
  [] -> sand 
  _ -> S.insert (last sand_path) sand
  where
    blocked = rock <> sand

    is_blocked (x,y) = 
      y == abyss_level + 1 || S.member (x,y) blocked

    sand_path = unfoldr (step is_blocked) entry

    abyss_level = abyssLevel rock

    -- falls_to_abyss = isJust $ find ((abyss_level <=) . snd) sand_path



main :: IO ()
main = do
  input <- parseHardError parser <$> readFile "input/14.txt"

  let entry = (500,0)
  let rock = pathPoints input

  print $ S.size $ converge (spawnSand entry rock) S.empty