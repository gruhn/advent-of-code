module Main where
import Data.Void (Void)
import Text.Megaparsec (Parsec, sepBy, (<|>), parse, errorBundlePretty)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (char, newline)
import qualified Data.Set as Set

data Move = R Int | U Int | L Int | D Int
    deriving (Show)

parser :: Parsec Void String ([Move], [Move])
parser = 
    let right = R <$> (char 'R' *> decimal)
        up = U <$> (char 'U' *> decimal)
        left = L <$> (char 'L' *> decimal)
        down = D <$> (char 'D' *> decimal)

        move = right <|> up <|> left <|> down
        moves = move `sepBy` char ','

    in (,) <$> moves <* newline <*> moves

type Point = (Int, Int)

line :: Point -> Point -> [Point]
line start end =
    let (x0,y0) = start
        (xn,yn) = end

        dx = signum (xn - x0)
        dy = signum (yn - y0)

        step (x,y) = (x+dx, y+dy)

    in takeWhile (/= end) (iterate step start)

applyMove :: Point -> Move -> Point
applyMove (x,y) (R dx) = (x+dx, y)
applyMove (x,y) (L dx) = (x-dx, y)
applyMove (x,y) (D dy) = (x, y+dy)
applyMove (x,y) (U dy) = (x, y-dy)

path :: Point -> [Move] -> [Point]
path origin [] = [origin]
path origin (move:moves) = 
    let target = origin `applyMove` move
    in line origin target <> path target moves

manhattanDist :: Point -> Int
manhattanDist (x,y) = abs x + abs y

stepDist :: Point -> [Point] -> Int
stepDist target = length . takeWhile (/= target)

main :: IO ()
main = do 
    input <- parse parser "" <$> readFile "2019/input/03.txt"

    case input of
        Left error -> putStr (errorBundlePretty error)
        Right (moves1, moves2) -> do
            let path1 = path (0,0) moves1
                path2 = path (0,0) moves2
                intersec = Set.intersection (Set.fromList path1) (Set.fromList path2)
                intersec' = Set.delete (0,0) intersec

            putStr "Part 1: "
            print $ Set.findMin $ Set.map manhattanDist intersec'

            let combinedStepDist target = stepDist target path1 + stepDist target path2 

            putStr "Part 2: "
            print $ Set.findMin $ Set.map combinedStepDist intersec'
