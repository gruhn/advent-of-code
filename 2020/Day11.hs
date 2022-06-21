module Day11 (parser, solver) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Void (Void)
import Text.Megaparsec (Parsec, sepBy, some, (<|>))
import Text.Megaparsec.Char (char, newline)

tupleAdd :: (Int,Int) -> (Int,Int) -> (Int,Int)
tupleAdd (x,y) (x',y') = (x+x',y+y')

line :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
line start gradient = iterate (tupleAdd gradient) start

countElem :: Eq a => a -> [a] -> Int
countElem a = length . filter (== a)

converge :: Eq a => a -> (a -> a) -> a
converge a f
    | a == f a  = a
    | otherwise = converge (f a) f

takeJust :: [Maybe a] -> [a]
takeJust [] = []
takeJust (Nothing : _) = []
takeJust (Just a : as) = a : takeJust as

data Star a = Star 
    { center :: a, directions :: [[a]] }

type Grid a = Map (Int, Int) a

fromNestedList :: [[a]] -> Grid a
fromNestedList = go 0 0 where
    go x y []           = Map.empty
    go x y ([]:rss)     = go 0 (y+1) rss
    go x y ((r:rs):rss) =
        Map.insert (x,y) r $ go (x+1) y (rs:rss)

starAt :: a -> Grid a -> (Int, Int) -> Star a
starAt def grid (x,y) = 
    let valueAt (x',y') = Map.lookup (x',y') grid

        gradients = 
            [ (-1,-1), (0,-1), (1,-1)
            , (-1, 0),         (1, 0)
            , (-1, 1), (0, 1), (1, 1) ]

        direction = takeJust . map valueAt . tail . line (x,y)

        center = Map.findWithDefault def (x,y) grid
        directions = map direction gradients
        
    in Star center directions

mapWithStar :: a -> (Star a -> b) -> Grid a -> Grid b
mapWithStar def f grid =
    Map.mapWithKey (\k _ -> f $ starAt def grid k) grid

neighbors :: Star a -> [a]
neighbors (Star _ dirs) = 
    concatMap (take 1) dirs

data Seat = Occupied | Empty | Floor
    deriving (Eq, Show)

occupiedNeighbors :: Star Seat -> Int
occupiedNeighbors =
    countElem Occupied . neighbors

convPart1 :: Star Seat -> Seat
convPart1 s@(Star Empty _)
    | occupiedNeighbors s == 0 = Occupied
    | otherwise                = Empty
convPart1 s@(Star Occupied _)
    | occupiedNeighbors s >= 4 = Empty
    | otherwise                = Occupied
convPart1 s@(Star Floor _) = Floor

firstVisible :: [Seat] -> Seat
firstVisible dir =
    case dropWhile (== Floor) dir of
        []       -> Floor
        (seat:_) -> seat

occupiedVisible :: Star Seat -> Int
occupiedVisible = 
    countElem Occupied . map firstVisible . directions

convPart2 :: Star Seat -> Seat
convPart2 s@(Star Empty _)
    | occupiedVisible s == 0 = Occupied
    | otherwise              = Empty
convPart2 s@(Star Occupied _)
    | occupiedVisible s >= 5 = Empty
    | otherwise              = Occupied
convPart2 s@(Star Floor _) = Floor

parser :: Parsec Void String (Grid Seat)
parser =
    let emptySeat = Empty <$ char 'L'
        occupied = Occupied <$ char '#'
        floor = Floor <$ char '.'

        seat = occupied <|> emptySeat <|> floor

        row = some seat
        rows = row `sepBy` newline
    in fromNestedList <$> rows

countOccupied :: Grid Seat -> Int
countOccupied = Map.size . Map.filter (== Occupied)

solver :: Grid Seat -> IO ()
solver grid = do
    putStr "Part 1: "
    print $ countOccupied $ converge grid (mapWithStar Floor convPart1)

    putStr "Part 2: "
    print $ countOccupied $ converge grid (mapWithStar Floor convPart2)