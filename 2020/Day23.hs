module Main where

import Data.Function ((&))
import Data.Foldable (for_)
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.List ((\\))
import Data.Functor ((<&>))

type State = (Int, Vector Int)

successors :: State -> [Int]
successors (i, cups) = 
    let succ = cups V.! i
    in  succ : successors (succ, cups)

destinations :: State -> [Int]
destinations (i, cups) =
    let dest = (i-1) `mod` V.length cups
    in  dest : destinations (dest, cups)

move :: State -> State
move state@(i0, cups) = 
    let [i1,i2,i3,i4] = take 4 (successors state)

        dest = head (destinations state \\ [i1,i2,i3])
        destSucc = cups V.! dest

        -- FROM: ... i0 i1 i2 i3 i4 ... dest destSucc ...
        -- TO  : ... i0 i4 ... dest i1 i2 i3 destSucc ...

        cups' = cups V.// [ (i0,i4), (dest,i1), (i3,destSucc) ]

    in  (i4, cups')

initState :: [Int] -> State
initState xs =
    let -- convert to 0-based indices
        xs' = fmap (\x -> x - 1) xs
        successorOf i = (!! 1) $ dropWhile (/= i) (cycle xs')
        cups = V.generate (length xs') successorOf
    in  (head xs', cups)

fromDigits :: [Int] -> Int
fromDigits [] = 0
fromDigits (d:ds) =
    d * 10^length ds + fromDigits ds

resolveState :: State -> [Int]
resolveState (_, cups) = successors (0, cups)
    & take (V.length cups - 1)
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