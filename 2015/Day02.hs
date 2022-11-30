module Main where
import Text.Megaparsec (Parsec, sepBy, parse, errorBundlePretty)
import Data.Void (Void)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Functor ((<&>))
import Data.Function ((&))

type Cuboid = (Int, Int, Int)

parser :: Parsec Void String [Cuboid]
parser = 
    let cuboid = do
            x <- decimal
            char 'x'
            y <- decimal
            char 'x'
            z <- decimal
            return (x,y,z)
    in  cuboid `sepBy` newline

paper :: Cuboid -> Int
paper (x,y,z) =
    let sides = [x*y, x*z, y*z]
    in  sum sides * 2 + minimum sides

ribbon :: Cuboid -> Int
ribbon (x,y,z) = 
    let volume = x*y*z
        perimeters = (*2) <$> [ x+y, x+z, y+z ]
    in  volume + minimum perimeters

main :: IO ()
main = do
    input <- parse parser "" <$> readFile "input/02.txt"
    case input of 
        Left error -> putStr (errorBundlePretty error)
        Right cuboids -> do
            putStr "Part 1: "
            print $ cuboids <&> paper & sum

            putStr "Part 2: "
            print $ cuboids <&> ribbon & sum
