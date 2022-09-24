module Main where
import Data.List (group)
import Data.Char (intToDigit)

data Move = U | R | D | L
  deriving Show

keyPad1CodeAt :: (Int,Int) -> Char
keyPad1CodeAt (x,y) = intToDigit $ y*3 + x + 1

move1 :: (Int,Int) -> Move -> (Int,Int)
move1 (x, y) U = (x          , max (y-1) 0)
move1 (x, y) R = (min (x+1) 2, y          )
move1 (x, y) D = (x          , min (y+1) 2)
move1 (x, y) L = (max (x-1) 0, y          )

codesWith :: ((Int,Int) -> Move -> (Int,Int)) -> (Int,Int) -> [[Move]] -> [(Int,Int)]
codesWith go start = tail . scanl (foldl go) start

keyPad2CodeAt :: (Int, Int) -> Char
keyPad2CodeAt (x,y) = 
  [ "  1  "
  , " 234 "
  , "56789"
  , " ABC "
  , "  D  " ] !! y !! x

move2 :: (Int,Int) -> Move -> (Int,Int)
move2 (x, y) m = bound (step m)
  where
    step U = (x  , y-1)
    step R = (x+1, y  )
    step D = (x  , y+1)
    step L = (x-1, y  )

    bound (x',y') = (bound_x x', bound_y y')
      where
        bound_x x' = min (max min_x x') max_x
        bound_y y' = min (max min_y y') max_y

        max_y = 4 - abs (x - 2) 
        max_x = 4 - abs (y - 2)
        min_y = abs (x - 2)
        min_x = abs (y - 2)

parse :: String -> [Move]
parse = fmap go
  where
    go :: Char -> Move
    go char =
      case char of
        'U' -> U; 'R' -> R;
        'D' -> D; 'L' -> L;
        _ -> error "undefined direction"

main :: IO ()
main = do
  moves <- fmap parse . lines <$> readFile "2016/input/02.txt"

  putStr "Part 1: "
  print $ keyPad1CodeAt <$> codesWith move1 (1,1) moves

  putStr "Part 2: "
  print $ keyPad2CodeAt <$> codesWith move2 (0,2) moves