module Day23 where

import ParseUtil (Grid(..), gridP)
import Text.Parsec (char, (<|>))
import Text.Parsec.String (Parser, parseFromFile)
import qualified Data.Map as Map
import Data.Either

data Cell = South | East
    deriving Eq

type GridWithBounds = (Grid Cell, (Int, Int))

instance Show Cell where
    show South = "v"
    show East = ">"

cellP :: Parser (Maybe Cell)
cellP = (Nothing <$ char '.') 
    <|> (Just South <$ char 'v')
    <|> (Just East <$ char '>')

show' :: GridWithBounds -> String
show' (Grid grid, bounds) =
    show . Grid $ Map.insertWith (\_ x -> x) bounds South grid

day23 :: IO ()
day23 = do
    input <- parseFromFile (gridP cellP) "23-input.txt"
    let gwb = do  
            (Grid grid) <- input
            return
                ( Grid $ Map.mapMaybe id grid
                , fst $ Map.findMax grid
                )

    putStr "Part 1: "
    -- putStrLn ""
    -- putStr $ case converge step <$> gwb of
    --     Right x -> unlines $ map show' x
    --     Left _ -> "error"
    print $ length . converge step <$> gwb
    putStr "Part 2: "

converge :: Eq a => (a -> a) -> a -> [a]
converge f a 
    | a == f a  = [a]
    | otherwise = a : converge f (f a)

move :: GridWithBounds -> Cell -> (Int, Int) -> (Int, Int)
move gwb@(Grid grid, (xn,yn)) East (x,y) =
    let current = Map.lookup (x,y) grid
        newPos = ((x+1) `mod` (xn+1), y)
        target = Map.lookup newPos grid
    in case (current, target) of
        (Just East, Nothing) -> newPos
        _                    -> (x,y)
move gwb@(Grid grid, (xn,yn)) South (x,y) =
    let current = Map.lookup (x,y) grid
        newPos = (x, (y+1) `mod` (yn+1))
        target = Map.lookup newPos grid
    in case (current, target) of
        (Just South, Nothing) -> newPos
        _                     -> (x,y)

step :: GridWithBounds -> GridWithBounds
step gwb@(Grid grid, bounds) = 
    let afterEast = Map.mapKeys (move gwb East) grid
        gwb' = (Grid afterEast, bounds)
        afterSouth = Map.mapKeys (move gwb' South) afterEast
        gwb'' = (Grid afterSouth, bounds)
    in gwb''