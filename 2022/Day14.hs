module Main where
import Utils (Parser, parseHardError, converge)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (newline, string, char)
import Text.Megaparsec (sepBy)
import qualified Data.HashSet as S
import Data.HashSet (HashSet)
import Data.Foldable (find)

type Point = (Int,Int)
type Path = [Point]

parser :: Parser [Path]
parser = path `sepBy` newline
  where
    path :: Parser Path
    path = point `sepBy` string " -> "

    point :: Parser Point
    point = (,) <$> decimal <* char ',' <*> decimal

expandPath :: Path -> Path
expandPath [] = []
expandPath [p] = [p]
expandPath (p1:p2:ps) = segment p1 p2 <> expandPath (p2:ps)

segment :: Point -> Point -> Path
segment start@(x1,y1) end@(x2,y2) = takeWhile (/= end) $ iterate shift start
  where
    dx = signum (x1 - x2)
    dy = signum (y1 - y2)
    shift (x,y) = (x-dx, y-dy)

stepSand :: (Point -> Bool) -> Point -> Point
stepSand is_free (x,y) =
  case filter is_free [(x,y+1),(x-1,y+1),(x+1,y+1)] of
    []       -> (x,y)
    (next:_) -> next

spawnSand :: Point -> (Point -> Bool) -> HashSet Point -> HashSet Point
spawnSand entry_point is_rock sand = S.insert final_point sand
  where
    is_free (x,y) = not (is_rock (x,y) || S.member (x,y) sand)
    final_point = converge (stepSand is_free) entry_point

main :: IO ()
main = do
  rock_paths <- parseHardError parser <$> readFile "input/14.txt"

  let entry_point = (500, 0)
      rock_points = S.unions (S.fromList . expandPath <$> rock_paths)
      floor_level = maximum (S.map snd rock_points) + 2
      is_rock point = S.member point rock_points || snd point == floor_level

      all_steps = iterate (spawnSand entry_point is_rock) S.empty

  putStr "Part 1: "
  let at_floor_level point = snd point == floor_level - 1
  print $ length (takeWhile (not . any at_floor_level) all_steps) - 1

  putStr "Part 2: "
  let entry_not_blocked = not . S.member entry_point
  print $ length (takeWhile entry_not_blocked all_steps)