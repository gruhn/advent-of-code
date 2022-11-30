module Main where
import Text.Megaparsec (Parsec, choice, sepBy, parse, errorBundlePretty)
import Data.Void (Void)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (char, string, newline)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)

type Point = (Int,Int)

data Instr
    = On Point Point
    | Off Point Point
    | Toggle Point Point
    deriving Show

type Parser a = Parsec Void String a

parser :: Parser [Instr]
parser =
    let point :: Parser Point
        point = (,) <$> decimal <* char ',' <*> decimal

        instr :: (Point -> Point -> Instr) -> Parser Instr
        instr constr = constr <$> point <* string " through " <*> point

        on     = string "turn on "  *> instr On
        off    = string "turn off " *> instr Off
        toggle = string "toggle "   *> instr Toggle

    in  choice [on, off, toggle] `sepBy` newline

-- >>> points (499,499) (500,500)
-- fromList [(499,499),(499,500),(500,499),(500,500)]

points :: Point -> Point -> [Point]
points (x0,y0) (xn,yn) =
    [ (x,y) | x <- [x0..xn], y <- [y0..yn] ]

symDiff :: Ord a => Set a -> Set a -> Set a
symDiff s1 s2 =
    (s1 Set.\\ s2) `Set.union` (s2 Set.\\ s1)

apply1 :: Set Point -> Instr -> Set Point
apply1 grid (On p1 p2) =
    grid `Set.union` Set.fromList (points p1 p2)
apply1 grid (Off p1 p2) =
    grid Set.\\ Set.fromList (points p1 p2)
apply1 grid (Toggle p1 p2) =
    grid `symDiff` Set.fromList (points p1 p2)

apply2 :: Map Point Int -> Instr -> Map Point Int
apply2 grid (On p1 p2) = 
    let inc1 p = Map.insertWith (+) p 1
    in  foldr inc1 grid (points p1 p2)
apply2 grid (Off p1 p2) = 
    let dec1 p = Map.adjust (\v -> max 0 (v-1)) p
    in  foldr dec1 grid (points p1 p2)
apply2 grid (Toggle p1 p2) =
    let inc2 p = Map.insertWith (+) p 2
    in  foldr inc2 grid (points p1 p2)

main :: IO ()
main = do
    input <- parse parser "" <$> readFile "input/06.txt"
    case input of
        Left error -> putStr (errorBundlePretty error)
        Right input -> do
            putStr "Part 1: "
            print $ Set.size $ foldl apply1 Set.empty input

            putStr "Part 2: "
            print $ sum $ foldl apply2 Map.empty input