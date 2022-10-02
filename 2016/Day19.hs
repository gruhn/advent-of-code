module Main where

import qualified Data.Sequence as S
import Data.Sequence ((|>), Seq (Empty, (:<|)))

import Data.List (groupBy)

part1Slow :: Int -> Int
part1Slow elf_count = go $ S.fromList [1..elf_count]
  where
    go :: S.Seq Int -> Int
    go Empty = undefined 
    go (x :<| Empty)    = x
    go (x :<| _ :<| xs) = go (xs |> x)

part1Fast :: Int -> Int
part1Fast elf_count -- = (elf_count - 2^number_of_rounds + 1) * 2 - 1
                       = 2*elf_count - 2^(number_of_rounds+1) + 1
  where
    number_of_rounds = floor . logBase 2 . fromIntegral $ elf_count

-- prop> \(elf_count :: Int) -> elf_count < 1 || part1Fast elf_count == part1Slow elf_count
-- +++ OK, passed 100 tests.

part2Slow :: Int -> Int
part2Slow elf_count = go $ S.fromList [1..elf_count]
  where
    go :: S.Seq Int -> Int
    go S.Empty           = undefined 
    go (x S.:<| S.Empty) = x
    go xs                = go (xs' S.|> x')
      where
        (x' S.:<| xs') = S.deleteAt (length xs `div` 2) xs

-- >>> groupBy (<) (part2Slow <$> [1..40])
-- [[1],[1,3],[1,2,3,5,7,9],[1,2,3,4,5,6,7,8,9,11,13,15,17,19,21,23,25,27],[1,2,3,4,5,6,7,8,9,10,11,12,13]]

-- >>> groupBy (<) (part2Fast <$> [1..40])
-- [[1],[1,3],[1,2,3,5,7,9],[1,2,3,4,5,6,7,8,9,11,13,15,17,19,21,23,25,27],[1,2,3,4,5,6,7,8,9,10,11,12,13]]

-- >>> fmap length $ groupBy (<) $ part2Slow <$> [1..729]
-- [1,2,6,18,54,162,486]

-- >>> take 10 $ ((2*) . (3^)) <$> [0..]
-- [2,6,18,54,162,486,1458,4374,13122,39366]

part2Fast :: Int -> Int
part2Fast 1 = 1
part2Fast elf_count = if x <= 3^n then x else 2*x - 3^n
  where
    n = ceiling $ logBase 3 (fromIntegral elf_count) - 1
    x = elf_count - 3^n

-- prop> \(elf_count :: Int) -> elf_count < 1 || part2Fast elf_count == part2Slow elf_count
-- +++ OK, passed 100 tests.

main :: IO ()
main = do
  putStr "Part 1: "
  print $ part1Fast 3005290

  putStr "Part 2: "
  print $ part2Fast 3005290
