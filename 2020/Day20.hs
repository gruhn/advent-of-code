module Main where

import Data.List (transpose)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import Text.Megaparsec (Parsec, (<|>), some, sepEndBy, sepBy, parse, errorBundlePretty)
import Data.Void
import Text.Megaparsec.Char (string, char, newline)
import Text.Megaparsec.Char.Lexer (decimal, lexeme)
import Data.Either.Extra (fromEither)

data Tile = Tile 
    { tileIds :: Int
    , north :: String
    , east :: String 
    , south :: String 
    , west :: String 
    } deriving Show

fromList :: Int -> [String] -> Tile
fromList tileId rows = 
    let north = head rows
        south = last rows
        cols = transpose rows
        west = head cols
        east = last cols
    in Tile tileId north east south west

parser :: Parsec Void String [Tile]
parser =
    let tileId = string "Tile " *> decimal <* string ":\n"
        tileCell = char '#' <|> char '.'
        tileRow = some tileCell
        tileGrid = tileRow `sepEndBy` newline
        tile = fromList <$> tileId <*> tileGrid
    in tile `sepBy` newline

-- flipX :: Tile -> Tile
-- flipX (Tile tids north east south west) =
--     Tile (reverse tids) south (reverse east) north (reverse west)

-- flipY :: Tile -> Tile
-- flipY (Tile tids north east south west) =
--     Tile (map reverse tids) (reverse north) west (reverse south) east

-- rotate180 :: Tile -> Tile
-- rotate180 = flipX . flipY

-- appendEast :: Tile -> Tile -> Tile
-- appendEast (Tile tids1 n1 e1 s1 w1) (Tile tids2 n2 e2 s2 w2) = Tile
--     { tileIds = zipWith (++) tids1 tids2
--     , north = n1 ++ n2, east = e2
--     , south = s1 ++ s2, west = w1
--     }

-- appendSouth :: Tile -> Tile -> Tile
-- appendSouth (Tile tids1 n1 e1 s1 w1) (Tile tids2 n2 e2 s2 w2) = Tile
--     { tileIds = tids1 ++ tids2
--     , north = n1, east = e1 ++ e2
--     , south = s2, west = w1 ++ w2
--     }

presort :: [Tile] -> Map String [Int]
presort tiles =
    let accum :: Tile -> Map String [Int] -> Map String [Int]
        accum tile dict = 
            let Tile tid n e s w = tile
                dirs = [ n, e, s, w ]
                dirs' = map reverse dirs
                pairs = zip (dirs ++ dirs') (repeat [tid])
                dict' = Map.fromList pairs
            in Map.unionWith (++) dict dict'
    in foldr accum Map.empty tiles

f :: Map String [Int] -> Map Int [Int]
f dict = 
    let accum :: [Int] -> Map Int [Int] -> Map Int [Int]
        accum tids dict' = foldr (\tid dict'' -> Map.insertWith (++) tid (List.delete tid tids) dict'') dict' tids
    in Map.foldr accum Map.empty dict

solver :: [Tile] -> IO ()
solver tiles = do
    putStr "Part 1: "
    print 
        $ product
        $ Map.keys
        $ Map.filter ((==2) . length) 
        $ fmap List.nub 
        $ f 
        $ Map.filter ((==2) . length) 
        $ presort tiles

main :: IO ()
main = do
    input <- parse parser "" <$> readFile "2020/input/20.txt"

    case input of 
        Left error -> putStr (errorBundlePretty error)
        Right parsed -> solver parsed