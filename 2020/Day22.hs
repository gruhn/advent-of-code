module Main where

import Data.Foldable (for_, length)
import Data.Function ((&))
import qualified Data.Set as Set
import Data.MemoTrie (memo)

-- >>> takeUnique [1,2,3,6,4,4,4,4]
-- [1,2,3,6,4]

-- >>> takeUnique $ cycle [1,2,3]
-- [1,2,3]

takeUnique :: Ord a => [a] -> [a]
takeUnique = go Set.empty where
    go :: Ord a => Set.Set a -> [a] -> [a]
    go seen [] = []
    go seen (a:as)
        | Set.member a seen = []
        | otherwise = a : go (Set.insert a seen) as

type Config = ([Int], [Int])

startConfig' :: Config
startConfig' = ([9,2,6,3,1],[5,8,4,7,10])

startConfig :: Config
startConfig =
    ( [38,1,28,32,43,21,42,29,18,13,39,41,49,31,19,26,27,40,35,14,3,36,12,16,45]
    , [34,15,47,20,23,2,11,9,8,7,25,50,48,24,46,44,10,6,22,5,33,30,4,17,37] )

win' :: Config -> Bool
win' (as,[]) = True
win' ([],bs) = False
win' (a:as,b:bs)
    | length as >= a && length bs >= b =
        case last (play (as,bs)) of
            ([],_) -> False
            (_,[]) -> True
            (_,_)  -> True
    | a > b     = True
    | otherwise = False

step' :: Config -> Config
step' (as,[]) = (as,[])
step' ([],bs) = ([],bs)
step' config@(a:as,b:bs)
    | win config = (as ++ [a,b], bs)
    | otherwise  = (as, bs ++ [b,a])

play' :: Config -> [Config]
play' start = start 
    & iterate step
    & takeUnique

play = memo play'

step = memo step'

win = memo win'

score :: Config -> Int
score (as,bs) = as ++ bs
    & reverse
    & zipWith (*) [1..]
    & sum

main :: IO ()
main = do
    putStr "Part 2: "
    print (score . last . play $ startConfig)