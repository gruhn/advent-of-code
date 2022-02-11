module Day21 where

day21 :: IO ()
day21 = do 
    putStr "Part 1: "
    let result = head . dropWhile ((< 1000) . topScore) $ iterate step (0, 1, 5, 0, 0)
        (n, _, _, s1, s2) = result
    print result
    print (n*3 * min s1 s2)

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

-- frequencies = [ | sum <- [(1+1+1) .. (3+3+3)] ]
sums = [ sum [s1,s2,s3] | s1 <- [1,2,3], s2 <- [1,2,3], s3 <- [1,2,3] ]