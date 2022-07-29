module Main where
import Data.Function ((&))
import Data.Foldable (find)
import Data.Maybe (fromJust)
import qualified Data.Set as Set

primes :: [Int]
primes = 2 : filter isPrime [3..]

divides :: Int -> Int -> Bool
divides a b = (b `rem` a) == 0

isPrime :: Int -> Bool
isPrime n = primes
    & takeWhile (\p -> p*p <= n)
    & not . any (`divides` n)

primeFactors :: Int -> [Int]
primeFactors = go primes where 
    go :: [Int] -> Int -> [Int]
    go [] _ = undefined
    go (p:ps) m
        | m <= 1 = []
        | p `divides` m = p : go (p:ps) (m `div` p)
        | otherwise = go ps m

combinations :: [a] -> [[a]]
combinations [] = [[]]
combinations (a:as) =
    ((a:) <$> combinations as) ++ combinations as

divisors :: Int -> [Int]
divisors n = primeFactors n
    & combinations
    & fmap product
    & Set.fromList
    & Set.toList

presents :: Int -> Int
presents n =
    divisors n & sum & (*10)

main :: IO ()
main = do
    putStr "Part 1: "
    print $ [1..] & find ((34000000 <=) . presents)