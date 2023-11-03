module Main where
import Data.Function (on)
import Utils (count)

gen :: Int -> Int -> [Int]
gen factor = iterate next
 where next x = (x*factor) `rem` 2147483647

isMultipleOf :: Int -> Int -> Bool
isMultipleOf a b = b `rem` a == 0

match :: [Int] -> [Int] -> [Bool]
match = zipWith ((==) `on` (`rem` 2^16))

main :: IO ()
main = do
 let startA = 722
     startB = 354

     million = 1000000

 putStr "Part 1: "
 let genA = gen 16807 startA
 let genB = gen 48271 startB
 print $ count id $ take (40*million) $ match genA genB

 putStr "Part 2: "
 let genA' = filter (isMultipleOf 4) $ gen 16807 startA
 let genB' = filter (isMultipleOf 8) $ gen 48271 startB
 print $ count id $ take (5*million) $ match genA' genB'
