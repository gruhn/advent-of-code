module Main where

import Text.Megaparsec (Parsec, count, parse, errorBundlePretty, many)
import Data.Void (Void)
import Text.Megaparsec.Char (digitChar)
import Data.Char (digitToInt, intToDigit)
import Data.Function (on)
import Data.Foldable ( minimumBy, find, for_ )
import Data.List (transpose, intercalate)
import Data.List.Split (chunksOf)

data Pixel = Black | White | Trans
    deriving Eq

instance Show Pixel where
    show Black = "#"
    show _ = " "

type Layer = [Pixel]

pixelFrom :: Char -> Pixel
pixelFrom '1' = Black
pixelFrom '0' = White
pixelFrom  _  = Trans

layers :: Int -> Int -> Parsec Void String [Layer]
layers width height =
    let pixel = pixelFrom <$> digitChar
        layer = count (width*height) pixel
    in  many layer

countPixel :: Pixel -> Layer -> Int
countPixel p = length . filter (==p)

mergeLayers :: [Layer] -> Layer
mergeLayers =
    fmap (head . dropWhile (==Trans)) . transpose

showLayer :: Layer -> String
showLayer pixels = 
    intercalate "\n" $ chunksOf 25 $ concatMap show pixels

main :: IO ()
main = do
    input <- parse (layers 25 6) "" <$> readFile "input/08.txt"
    case input of
        Left error   -> putStr (errorBundlePretty error)
        Right layers -> do
            putStr "Part 1: "
            let l = minimumBy (compare `on` countPixel White) layers
            print (countPixel Black l * countPixel Trans l)

            putStrLn "Part 2: "
            putStrLn $ showLayer $ mergeLayers layers