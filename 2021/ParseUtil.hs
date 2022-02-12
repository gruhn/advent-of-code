module ParseUtil (Grid(..), gridP, splitOn) where

import Data.List (stripPrefix)
import qualified Data.Map as Map
import Text.Parsec.String (Parser)
import Text.Parsec (many, sepBy, newline, ParsecT)

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn sep list = aux list []
    where  
        aux [] segment = [reverse segment]
        aux (a:as) segment = 
            case stripPrefix sep (a:as) of
                Just suffix -> reverse segment : aux suffix []
                Nothing     -> aux as (a:segment)

newtype Grid a = Grid 
    { getGrid :: Map.Map (Int, Int) a }
    deriving Eq

instance Show a => Show (Grid a) where
    show (Grid grid) =
        let (xn, yn) = fst $ Map.findMax grid
            -- FIXME: what if `show` returns more than one char?
            showCell xy = maybe '.' (head . show) (Map.lookup xy grid)
            row y = [ showCell (x,y) | x <- [0..xn] ]
        in unlines [ row y | y <- [0..yn]]

fromNestedList :: [[a]] -> Grid a
fromNestedList as =
    let go (x,y) [] = Map.empty
        go (x,y) ([]:rows) = go (0,y+1) rows
        go (x,y) ((cell:row):rows) =
            Map.insert (x,y) cell $ go (x+1,y) (row:rows)
    in Grid $ go (0,0) as

-- gridP :: ParsecT Char u m a -> Parser (Grid a)
gridP :: Parser a -> Parser (Grid a)
gridP cellP = 
    fromNestedList <$> many cellP `sepBy` newline