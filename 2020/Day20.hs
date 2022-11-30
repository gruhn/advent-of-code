module Main where

import Data.List ( transpose, delete )
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import Text.Megaparsec (Parsec, (<|>), some, sepEndBy, sepBy, parse, errorBundlePretty)
import Data.Void ( Void )
import Text.Megaparsec.Char (string, char, newline)
import Text.Megaparsec.Char.Lexer (decimal, lexeme)
import Data.Either.Extra (fromEither)
import Data.Foldable (minimumBy, for_)
import Data.Function (on)
import qualified Data.Set as Set
import Data.List.Extra (intercalate)

data Tile = Tile { tileIds :: [[Int]], getTile :: [String] }
instance Show Tile where
    show = intercalate "\n" . getTile

parser :: Parsec Void String [Tile]
parser =
    let mkTile tid grid = Tile [[tid]] grid
        
        tileId = string "Tile " *> decimal <* string ":\n"
        tileCell = char '#' <|> char '.'
        tileRow = some tileCell
        tileGrid = tileRow `sepEndBy` newline
        tile = mkTile <$> tileId <*> tileGrid

    in  tile `sepBy` newline

northEdge :: Tile -> String
northEdge (Tile _ tile) = head tile

southEdge :: Tile -> String
southEdge (Tile _ tile) = last tile

flipY :: Tile -> Tile
flipY (Tile ids grid) = Tile
    (fmap reverse ids)
    (fmap reverse grid)

rotate90 :: Tile -> Tile
rotate90 (Tile ids grid) = Tile 
    (reverse . transpose $ ids)
    (reverse . transpose $ grid)

tileModesOf :: Tile -> [Tile]
tileModesOf tile =
    scanl (flip ($)) tile
        [ rotate90, rotate90, rotate90
        , flipY
        , rotate90, rotate90, rotate90
        ]

instance Eq Tile where
    t1 == t2 = tileIds t1 == tileIds t2
instance Ord Tile where
    t1 <= t2 = tileIds t1 <= tileIds t2

type TilePool = Map.Map String [Tile]

dropTile :: Tile -> TilePool -> TilePool
dropTile tile = Map.map (delete tile)

tilePoolFrom :: [Tile] -> TilePool
tilePoolFrom tiles =
    let insert tile = Map.insertWith (++) (southEdge tile) [tile]
    in  foldr insert Map.empty tiles

join :: Tile -> Tile -> Tile
join (Tile ids1 grid1) (Tile ids2 grid2) = Tile
    (ids1 ++ ids2)
    (init grid1 ++ tail grid2)

buildColumn :: TilePool -> Tile -> [Tile]
buildColumn tilePool column =
    let topPieces = Map.findWithDefault [] (northEdge column) tilePool
        columnsWith top =
            buildColumn (dropTile top tilePool) (top `join` column) 
        newColumns = concatMap columnsWith topPieces
    in  if null topPieces then 
            [column] 
        else 
            newColumns

buildColumns :: [Tile] -> [Tile]
buildColumns tiles =
    let tilePool = tilePoolFrom tiles

        dropBottomEdge (Tile ids t) = Tile ids (init t)
        dropTopEdge (Tile ids t) = Tile ids (tail t)

        boundaryTiles = fmap dropBottomEdge
            $ concatMap snd
            $ Map.toList
            $ Map.filter ((== 1) . length) tilePool

        columnFrom startTile =
            buildColumn (dropTile startTile tilePool) startTile

    in  dropTopEdge <$> concatMap columnFrom boundaryTiles

seaMonster :: [String]
seaMonster =
    [ "                  # "
    , "#    ##    ##    ###"
    , " #  #  #  #  #  #   " ]

matchChar :: Char -> Char -> Bool
matchChar '#' '#' = True
matchChar '#'  _  = False
matchChar  _   _  = True

matchPattern :: [String] -> [String] -> Bool
matchPattern pattern window = 
    and . concat $ zipWith (zipWith matchChar) pattern window

convolute :: (Int, Int) -> ([[a]] -> b) -> [[a]] -> [[b]]
convolute (w,h) f grid
    | length grid < h = []
    | otherwise =
        let go rows 
                | length (head rows) < w = []
                | otherwise = f (take w <$> rows) : go (drop 1 <$> rows)
        in  go (take h grid) : convolute (w,h) f (drop 1 grid)

dims :: [[a]] -> (Int,Int)
dims grid = (length (head grid), length grid)
    
main :: IO ()
main = do
    input <- parse parser "" <$> readFile "input/20.txt"
    case input of
        Left error -> putStr (errorBundlePretty error)
        Right tiles -> do
            let tileModes = concatMap tileModesOf tiles
                columns = buildColumns tileModes
                rows = rotate90 <$> columns
                results = buildColumns rows

            putStr "Part 1: "
            let resultIds = tileIds $ head results
                accessCornerIds = [head.head, last.head, head.last, last.last]
                cornerIds = ($ resultIds) <$> accessCornerIds
            print (product cornerIds)

            putStr "Part 2: "
            let window = dims seaMonster
                countSeaMonsters = length 
                    . filter id 
                    . concat 
                    . convolute window (matchPattern seaMonster)

                seaMonsterCount =
                    maximum $ countSeaMonsters . getTile <$> results

                countHashes = 
                    length . filter (=='#') . concat

                totalHashes = countHashes . getTile . head $ results
                seaMonsterHashes = countHashes seaMonster

            print (totalHashes - seaMonsterCount * seaMonsterHashes)