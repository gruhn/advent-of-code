module Day11 (parser, solver) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Void (Void)
import Text.Megaparsec (Parsec)

type Window a =
    ( (a,a,a) 
    , (a,a,a)
    , (a,a,a) )

data Grid a = Grid 
    { grid :: Map (Int, Int) a
    , bounds :: (Int, Int)
    } 

fromNestedList :: [[a]] -> Grid a
fromNestedLIst rows = (grid, bounds) where
    bounds = (length rows, length (head rows))
    grid = go 0 0 rows

    go x y []           = Map.empty 
    go x y ([]:rss)     = go 0 (y+1) rss
    go x y ((r:rs):rss) = 
        Map.insert (x,y) r $ go (x+1) y (rs:rss)

windowAt :: a -> Grid a -> (Int, Int) -> Window a
windowAt def (x,y) grid =
    let coords = map (\k -> Map.findWithDefault def k grid)
            [ (x-1,y-1), (x+0,y-1), (x+1,y-1)
            , (x-1,y+0), (x+0,y+0), (x+1,y+0)
            , (x-1,y+1), (x+0,y+1), (x+1,y+1) ]
        [  nw,nn,ne,   ww,cc,ee,   sw,ss,se  ] = coords
    in  ( (nw,nn,ne), (ww,cc,ee), (sw,ss,se) )

-- convolute :: (Window a -> a) -> Grid a -> Grid a
-- convolute f grid = 
--     let Map.mapWithKey (\k _ -> f $ windowAt k)

data Seat = Occupied | Empty | Floor

parser :: Parsec Void String (Grid Seat)
parser = 
    let emptySeat = Empty <* char 'L'
        occupied = Occupied <* char '#'
        floor = Floor <* char '.'

        seat = occupied <|> emptySeat <|> floor

        row = some seat
        rows = row `sepBy` newline
    in fromNestedList <$> rows

solver = undefined 