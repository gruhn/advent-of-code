module Main where

import Data.Foldable (for_, length, Foldable (toList))
import Data.Function ((&))
import qualified Data.Set as Set

takeUnique :: Ord a => [a] -> [a]
takeUnique = go Set.empty where
    go :: Ord a => Set.Set a -> [a] -> [a]
    go seen [] = []
    go seen (a:as)
        | Set.member a seen = []
        | otherwise = a : go (Set.insert a seen) as

type Config = ([Int], [Int])

winsFrom :: Config -> Bool
winsFrom (as,[]) = True
winsFrom ([],bs) = False
winsFrom (a:as, b:bs)
    | length as >= a && length bs >= b =
        case last (play (take a as, take b bs)) of
            ([],_) -> False
            (_,[]) -> True
            (_,_)  -> True
    | a > b     = True
    | otherwise = False

step :: Config -> Config
step (as,[]) = (as, [])
step ([],bs) = ([],bs)
step config@(a:as, b:bs)
    | winsFrom config = (as ++ [a,b], bs)
    | otherwise       = (as, bs ++ [b,a])

play :: Config -> [Config]
play start = start 
    & iterate step
    & takeUnique

score :: Config -> Int
score (as,bs) = (as <> bs)
    & toList
    & reverse
    & zipWith (*) [1..]
    & sum

start :: Config
start =
    ( [38,1,28,32,43,21,42,29,18,13,39,41,49,31,19,26,27,40,35,14,3,36,12,16,45]
    , [34,15,47,20,23,2,11,9,8,7,25,50,48,24,46,44,10,6,22,5,33,30,4,17,37] )

main :: IO ()
main = do 
  let game = play start
  for_ game print

  putStr "Part 2: "
  print (game & last & score)