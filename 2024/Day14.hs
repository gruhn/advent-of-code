module Main (main) where
import Utils (Parser, integer, parseFile, showGrid2D)
import Text.Megaparsec (sepEndBy)
import Text.Megaparsec.Char (string, char, newline)
import Data.Foldable (find, traverse_)
import qualified Data.Map as Map
import Data.Maybe (maybeToList, mapMaybe)
import Data.List (sort, group, scanl')

type Vec = (Int, Int)

type Config = (Vec, Vec)

parser :: Parser [Config]
parser = config `sepEndBy` newline
  where
    config :: Parser Config
    config = do
      pos <- string "p=" *> vec
      vel <- string "v=" *> vec
      return (pos, vel)

    vec :: Parser Vec
    vec = (,) <$> integer <* char ',' <*> integer

step :: Vec -> Config -> Config
step (width, height) ((px, py), vel@(vx, vy)) =
  let
    px' = (px+vx) `mod` width
    py' = (py+vy) `mod` height
  in
    ((px', py'), vel)

data Quadrant = TopLeft | TopRight | BottomLeft | BottomRight
  deriving (Eq, Ord, Show)

quadrant :: (Int, Int) -> Config -> Maybe Quadrant
quadrant (width, height) ((x,y), _)
  | x < width `div` 2 && y < height `div` 2 = Just TopLeft
  | x < width `div` 2 && y > height `div` 2 = Just BottomLeft
  | x > width `div` 2 && y < height `div` 2 = Just TopRight
  | x > width `div` 2 && y > height `div` 2 = Just BottomRight
  | otherwise = Nothing

average :: [Int] -> Int
average xs = sum xs `div` length xs

variance :: [Int] -> Int
variance xs = average [ x^2 | x <- xs ] - average xs ^ 2

printState :: (Int, Int) -> (Int, [Config]) -> IO ()
printState (width, height) (time, state) = do
  let show_tile :: Int -> Int -> Char
      show_tile x y 
        | (x,y) `elem` map fst state = 'R'
        | otherwise = '.'

  putStrLn ""
  putStrLn $ showGrid2D show_tile width height
  putStrLn $ "Time: " ++ show time

scanMinVariance :: Int -> [(Int, [Config])] -> [(Int, [Config])]
scanMinVariance prev_min_variance [] = []
scanMinVariance prev_min_variance ((time, state) : timeline) =
  let 
    (xs, ys) = unzip $ map fst state 
    total_variance = variance xs + variance ys
  in  
    if total_variance < prev_min_variance then
      (time, state) : scanMinVariance total_variance timeline
    else
      scanMinVariance prev_min_variance timeline

main :: IO ()
main = do
  input <- parseFile parser "input/14.txt"

  let bounds :: Vec
      bounds = (101, 103) -- (11, 7)

      timeline :: [[Config]]
      timeline = iterate (map (step bounds)) input

  putStr "Part 1: "
  let state = timeline !! 100
      quadrants = mapMaybe (quadrant bounds) state
  print $ product $ map length $ group $ sort quadrants

  putStrLn "Part 2: "
  traverse_ (printState bounds)
    $ scanMinVariance maxBound 
    $ zip [0..] timeline
