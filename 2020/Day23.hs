module Main where

import Data.Function ((&))
import Data.List.PointedList.Circular (PointedList(PointedList), fromList, previous, next, deleteRight, insertRight, find)
import Data.List ((\\))
import Data.Maybe (fromMaybe, fromJust)
import Data.Foldable (for_)

takeNext :: Int -> PointedList a -> [a]
takeNext 0 pl = []
takeNext n pl@(PointedList _ c _) = 
    c : takeNext (n-1) (next pl)

dropNext :: Int -> PointedList a -> PointedList a
dropNext 0 pl = pl
dropNext n pl =
    case deleteRight pl of
        Just pl' -> dropNext (n-1) pl'
        Nothing  -> pl

insertAfter :: Eq a => a -> [a] -> PointedList a -> PointedList a
insertAfter a as pl =
    let insertAll pl = foldl (flip insertRight) pl as
    in  insertAll <$> find a pl

-- >>> dest 3 & take 10
-- [2,1,9,8,7,6,5,4,3,2]

dest :: Int -> [Int]
dest c = [1..9]
    & reverse 
    & cycle
    & dropWhile (/= c)
    & tail

move :: PointedList Int -> PointedList Int
move pl@(PointedList _ c _) =
    let picked = takeNext 3 pl
        dest' = head (dest c \\ picked)
    in  pl & dropNext 3 & insertAfter dest' picked & next

main :: IO ()
main = do 
    let input = fromJust $ fromList [3,8,9,1,2,5,4,6,7]
    for_ (iterate move input & take 100) print
