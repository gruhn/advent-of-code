module Main where

import Data.MemoTrie (memo3, mup)
import Data.Tuple (swap)

memo4 = mup memo3

topScore :: State -> Int
topScore (_, _, _, s1, s2) =
    max s1 s2

newPos :: Int -> Int -> Int
newPos n pos =
    let moves  = sum [ (n*3+s-1) `mod` 100 +1 | s <- [1,2,3] ]
    in (pos + moves - 1) `mod` 10 + 1

type State = (Int, Int, Int, Int, Int)

step :: State -> State
step (n, p0, p1, s0, s1) =
    if even n then
        (n+1, newPos n p0, p1, s0 + newPos n p0, s1)
    else 
        (n+1, p0, newPos n p1, s0, s1 + newPos n p1)

dieSums :: [Integer]
dieSums = [ s1+s2+s3 | s1 <- [1,2,3], s2 <- [1,2,3], s3 <- [1,2,3] ]

type State2 = (Integer, Integer, Integer, Integer)

move :: State2 -> Integer -> State2
move (p0,p1,s0,s1) x =
    let p0' = (p0+x-1) `rem` 10 +1
        -- p1' = (p1+x-1) `rem` 10 +1
    in (p0', p1, s0+p0', s1)

uncurry4 :: (a -> b -> c -> d -> e) -> (a,b,c,d) -> e
uncurry4 f (a,b,c,d) = f a b c d

winCount :: Integer -> Integer -> Integer -> Integer -> (Integer, Integer)
winCount p0 p1 s0 s1
    | s0 >= 21 = (1,0)
    | s1 >= 21 = (0,1)
    | otherwise = foldr addVec (0,0) nextStates where
        state = (p1,p0,s1,s0)
        nextStates = map (swap . uncurry4 winCountMemo . move state) dieSums
        addVec (a,b) (c,d) = (a+c,b+d)

winCountMemo = memo4 winCount

main :: IO ()
main = do 
    putStr "Part 1: "
    let result = head . dropWhile ((< 1000) . topScore) $ iterate step (0, 1, 5, 0, 0)
        (n, _, _, s1, s2) = result
    print (n*3 * min s1 s2)
    putStr "Part 2: "
    print $ uncurry max $ winCountMemo 5 1 0 0