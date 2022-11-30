module Main where
import Text.Megaparsec (Parsec, oneOf, choice, many, parse, errorBundlePretty)
import Data.Void (Void)
import Vec
import Text.Megaparsec.Char (char)
import qualified Data.Set as Set
import Data.Function ((&))

parser :: Parsec Void String [Vec2 Int]
parser = 
    let north = Vec2 (0,-1) <$ char '^'
        west = Vec2 (1,0) <$ char '>'
        south = Vec2 (0,1) <$ char 'v'
        east = Vec2 (-1,0) <$ char '<'
    in  many (choice [north, south, west, east])

tour :: [Vec2 Int] -> [Vec2 Int]
tour = scanl (+) (Vec2 (0,0))

countUnique :: Ord a => [a] -> Int
countUnique = Set.size . Set.fromList

takeEven :: [a] -> [a]
takeEven [] = []
takeEven [x0] = [x0]
takeEven (x0:x1:xs) =
    x0 : takeEven xs

takeOdd :: [a] -> [a]
takeOdd = takeEven . tail

main :: IO ()
main = do
    input <- parse parser "" <$> readFile "input/03.txt"
    case input of
        Left error -> putStr (errorBundlePretty error)
        Right dirs -> do
            putStr "Part 1: "
            print $ dirs & tour & countUnique

            putStr "Part 2: "
            let tour1 = dirs & takeEven & tour 
                tour2 = dirs & takeOdd & tour

            print $ countUnique (tour1 ++ tour2)