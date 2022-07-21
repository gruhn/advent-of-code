module Main where

import Data.Function ((&))
import Data.Foldable (for_)
import qualified Data.Vector.Mutable as V
import qualified Data.List as List

type State n = (Int, V.MVector n Int)

successors :: PrimState m => State n -> m [Int]
successors (i, cups) = do
    succ <- V.read cups i
    succs <- successors (succ, cups)
    return (succ : succs)

destinations :: State n -> [Int]
destinations (i, cups) =
    let dest = (i-1) `mod` length cups
    in  dest : destinations (dest, cups)

insertAfter :: State n -> [Int] -> V.MVector n Int
insertAfter (dest, cups) picked = do
    destSucc <- V.read cups dest
    V.write cups dest (head picked)
    V.write cups (last picked) destSucc

move :: State n -> State n
move state@(i, cups) = do
    picked <- take 3 <$> successors state 
    let dest = head (destinations state List.\\ picked)

    cups' <- insertAfter (dest, cups) picked
    lastSucc <- V.read cups' (last picked)
    cups'' <- V.write cups' i lastSucc
    i' <- head <$> successors (i, cups'')

    (i', cups'')

initState :: [Int] -> State n
initState xs = 
    let -- convert to 0-based indices
        xs' = fmap (\x -> x - 1) xs
        successorOf i = (!! 1) $ dropWhile (/= i) (cycle xs')
        state = V.generate (length xs') successorOf
    in  (head xs', state)

fromDigits :: [Int] -> Int
fromDigits [] = 0
fromDigits (d:ds) =
    d * 10^length ds + fromDigits ds

resolveState :: State n -> [Int]
resolveState (_, cups) = (0, cups)
    & successors
    & take (length cups - 1)
    -- convert back to 1-based indices
    & fmap (+1)

main :: IO ()
main = do 
    let input = [3,8,9,1,2,5,4,6,7]
                -- [3,2,6,5,1,9,4,7,8]

    putStr "Part 1: "
    let startState1 = initState input
        finalState1 = iterate move startState1 !! 100
    print (finalState1 & resolveState & fromDigits)

    -- putStr "Part 2: "
    -- let startState2 = initState (input ++ [10 .. 10^6])
    --     finalState2 = iterate move startState2 !! (10^3)
    -- print (finalState2 & resolveState & take 2)