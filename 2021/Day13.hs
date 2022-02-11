module Day13 where

import Text.Parsec
import Text.Parsec.String
import qualified Data.Set as Set
import Data.Bifunctor (first, second)
import Data.Either (fromRight)
import Data.List (intersperse)

data FoldInst = FoldX Int | FoldY Int
    deriving Show

puzzleInput :: Parser (Set.Set (Int, Int), [FoldInst])
puzzleInput = do
    dots <- Set.fromList <$> many pair
    _ <- newline
    ins <- many instruction
    return (dots, ins)

instruction :: Parser FoldInst
instruction = do
    _ <- string "fold along "
    axis <- oneOf ['x', 'y']
    _ <- char '='
    val <- nat
    _ <- newline
    case axis of
        'x' -> return $ FoldX val
        'y' -> return $ FoldY val
        _   -> fail ""

nat :: Parser Int
nat = read <$> many1 digit

pair :: Parser (Int, Int)
pair = do
    num1 <- nat
    _ <- char ','
    num2 <- nat
    _ <- newline
    return (num1, num2)

main :: IO ()
main = do
    inputRaw <- readFile "13-input.txt"
    let parsed = parse (puzzleInput <* eof) "" inputRaw
    let results = do
        (dots, ins) <- parsed
        return (scanl apply dots ins)

    putStr "Part 1: "
    print $ length . (!! 1) <$> results
    putStrLn "Part 2: "
    putStr 
        $ fromRight "parse error" 
        $ showDots . last
        <$> results

apply :: Set.Set (Int, Int) -> FoldInst -> Set.Set (Int, Int)
apply dots (FoldX axis)  =
    let leftDots = Set.filter ((<axis) . fst) dots
        rightDots = Set.filter ((>axis) . fst) dots
        rightDots' = Set.map (first ((2*axis) -)) rightDots
    in Set.union leftDots rightDots'
apply dots (FoldY axis) =
    let topDots = Set.filter ((<axis) . snd) dots
        bottomDots = Set.filter ((>axis) . snd) dots
        bottomDots' = Set.map (second ((2*axis) -)) bottomDots
    in Set.union topDots bottomDots'

showDots :: Set.Set (Int, Int) -> String
showDots dots =
    let nx = Set.findMax $ Set.map fst dots
        ny = Set.findMax $ Set.map snd dots
        showDot dot
            | Set.member dot dots = '#'
            | otherwise           = '.'
        row y = [ showDot (x, y) | x <- [0..nx] ]
    in unlines [ row y | y <- [0..ny]]