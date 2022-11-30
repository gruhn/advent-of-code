module Main where

import Text.Megaparsec (Parsec, sepBy, some, choice, parse, errorBundlePretty)
import Text.Megaparsec.Char (newline, string)
import Data.Void (Void)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Either.Extra (fromEither)
import Vec

type Tile = Vec2 Int

parser :: Parsec Void String [Tile]
parser = tiles where
    dir = Vec2 <$> choice
        [ (2,0)   <$ string "e"
        , (1,-1)  <$ string "se"
        , (-1,-1) <$ string "sw"
        , (-2,0)  <$ string "w"
        , (-1,1)  <$ string "nw"
        , (1,1)   <$ string "ne" ]
    tile = sum <$> some dir
    tiles = tile `sepBy` newline

flipTile :: Tile -> Set Tile -> Set Tile
flipTile tile tiles
    | Set.member tile tiles = Set.delete tile tiles
    | otherwise             = Set.insert tile tiles

neighbors :: Tile -> Set Tile
neighbors v =
    Set.fromList $ map ((+ v) . Vec2) [ (2,0), (1,-1), (-1,-1), (-2,0), (-1,1), (1,1) ]

convolute :: Set Tile -> Set Tile
convolute blackTiles =
    let whiteTiles = Set.unions (Set.map neighbors blackTiles) Set.\\ blackTiles

        blackNeighbors tile =
            length $ Set.intersection (neighbors tile) blackTiles

        blackRule n = not (n == 0 || n > 2)
        whiteRule n = n == 2

        keptBlackTiles = Set.filter (blackRule . blackNeighbors) blackTiles
        addedBlackTiles = Set.filter (whiteRule . blackNeighbors) whiteTiles

    in Set.union keptBlackTiles addedBlackTiles

solver :: [Tile] -> IO ()
solver coords = do
    let blackTiles = foldr flipTile Set.empty coords

    putStr "Part 1: "
    print $ length blackTiles

    putStr "Part 2: "
    print $ length $ (!! 100) $ iterate convolute blackTiles

main :: IO ()
main = do
    input <- parse parser "" <$> readFile "input/24.txt"
    case input of 
        Left error -> putStr (errorBundlePretty error)
        Right parsed -> solver parsed