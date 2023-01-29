module Main where
import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec (string, many1, (<|>), newline, sepBy, char, option, try)
import Text.Parsec.Char (digit)
import Data.Foldable (foldl')
import qualified Cuboid
import Cuboid ( Cuboid(Cuboid), Range(Range) )

parser :: Parser [(Bool, Cuboid)]
parser = cuboid `sepBy` newline
    where
        int :: Parser Int
        int = do 
            sign <- option "" (string "-")
            digits <- many1 digit
            return (read $ sign ++ digits)

        range :: Parser Range
        range = Range <$> int <* string ".." <*> int 

        cuboid_state :: Parser Bool
        cuboid_state = try on <|> off where
            on = True <$ string "on"
            off = False <$ string "off"

        cuboid :: Parser (Bool, Cuboid)
        cuboid = do
            state  <- cuboid_state
            xRange <- string " x=" *> range
            yRange <- string ",y=" *> range
            zRange <- string ",z=" *> range
            return (state, Cuboid [xRange, yRange, zRange])

main :: IO ()
main = do
    instructions <- parseFromFile parser "input/22.txt"

    let result_volume = sum . map Cuboid.volume . foldl' applyInst []
        init_area = Cuboid 
            [ Range (-50) 50 
            , Range (-50) 50
            , Range (-50) 50 ]

    putStr "Part 1: "
    print $ result_volume . filter ((init_area `Cuboid.contains`) . snd) <$> instructions
    putStr "Part 2: "
    print $ result_volume <$> instructions

applyInst :: [Cuboid] -> (Bool, Cuboid) -> [Cuboid]
applyInst cs (False, c) = concatMap (`Cuboid.subtract` c) cs
applyInst cs (True, c) = c : concatMap (`Cuboid.subtract` c) cs 
