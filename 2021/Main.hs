module Main where
import Day15 (day15)
import Day18 (day18)
import Day21 (day21)

main :: IO ()
main = day21

step (n, p0, p1)
    | even n    = (n+1, p0 + sum [ n*3+1, n*3+2, n*3+3 ], p1)
    | otherwise = (n+1, p0 + sum [ n*3+1, n*3+2, n*3+3 ], p1)