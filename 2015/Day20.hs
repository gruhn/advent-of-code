module Main where
import Data.Function ((&))
import Data.Foldable (find)
import Data.Maybe (fromJust)
import qualified Data.IntSet as Set

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
    combinations as ++ ((a:) <$> combinations as)

divisors :: Int -> [Int]
divisors n = primeFactors n
    & combinations
    & fmap product
    & Set.fromList
    & Set.toList

presents1 :: Int -> Int
presents1 n = primeFactors n 
    & combinations
    & fmap product
    & Set.fromList
    & Set.toList
    & sum 

presents2 :: Int -> Int
presents2 n = primeFactors n
    & combinations
    & fmap product
    & filter (<=50)
    & Set.fromList
    & Set.toList
    & fmap (n `div`)
    & sum

main :: IO ()
main = do
    putStr "Part 1: "
    print $ [1..] & find (((34000000 `div` 10) <=) . presents1)
    putStr "Part 2: "
    print $ [1..] & find (((34000000/11) <=) . fromIntegral . presents2)
