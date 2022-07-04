module Main where
import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec ( char, newline, choice, many1, sepBy, digit, letter )

type Vec = [Int]

add :: Vec -> Vec -> Vec
add = zipWith (+)

scale :: Int -> Vec -> Vec
scale s = map (s *)

rotate :: Int -> Vec -> Vec
rotate turns = (!! abs turns) . iterate rotate90 where
    sign = signum turns
    rotate90 = reverse . zipWith (*) [-sign, sign]

data Move = Trans Vec | Rotate90 Int | Forward Int
    deriving Show

moveFrom :: (Char, Int) -> Move
moveFrom pair = case pair of
    ('R', x) -> Rotate90 (x `div` 90)
    ('L', x) -> Rotate90 (-x `div` 90)
    ('N', x) -> Trans [0,x]
    ('E', x) -> Trans [x,0]
    ('S', x) -> Trans [0,-x]
    ('W', x) -> Trans [-x,0]
    ('F', x) -> Forward x
    _        -> error "parse error"

move :: Parser Move
move = do 
    moveType <- letter
    moveVal <- read <$> many1 digit
    return $ moveFrom (moveType, moveVal)

applyMove :: (Vec, Vec) -> Move -> (Vec, Vec)
applyMove (dir, v) move = case move of
    Rotate90 x -> (rotate x dir, v)
    Trans w    -> (dir, add v w)
    Forward x  -> (dir, add v (scale x dir))

applyMove' :: (Vec, Vec) -> Move -> (Vec, Vec)
applyMove' (dir, v) move = case move of
    Rotate90 x -> (rotate x dir, v)
    Trans w    -> (add dir w, v)
    Forward x  -> (dir, add v (scale x dir))

manhattanDist :: Vec -> Int
manhattanDist = sum . map abs

main :: IO ()
main = do
    input <- parseFromFile (move `sepBy` newline) "2020/input/12.txt"

    putStr "Part 1: "
    print $ manhattanDist . snd . 
        foldl applyMove ([1,0], [0,0]) <$> input

    putStr "Part 2: "
    let waypoint = [10,1]
    print $ manhattanDist . snd .
        foldl applyMove' (waypoint, [0,0]) <$> input