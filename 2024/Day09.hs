module Main (main) where

import Utils (chunksOf)
import Data.Char (digitToInt, intToDigit)
import Data.Sequence (Seq ((:<|), (:|>)))
import qualified Data.Sequence as Seq
import Data.Foldable (traverse_)
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Monadic (monadicIO, forAllM, monitor, run, pre, assert)
import Test.QuickCheck (quickCheck)
import System.Process (readProcessWithExitCode)
import Test.QuickCheck.Property ((===))
import Test.QuickCheck ((=/=))
import Test.QuickCheck.Arbitrary (genericShrink)
import Test.QuickCheck (shrinkIntegral)
import Test.QuickCheck (shrinkList)
import Debug.Trace (trace, traceShow, traceShowId)

parse :: String -> Seq (Int, Int, Int)
parse input = 
  let
    nums = chunksOf 2 $ (++ [0]) . map digitToInt . head . lines $ input
  in
    Seq.fromList $ zipWith (\bId [file, size] -> (bId, file, size)) [0..] nums

fill :: Seq (Int, Int, Int) -> Seq (Int, Int, Int)
fill Seq.Empty = Seq.Empty
fill ((bId, file, size) :<| Seq.Empty) = (bId, file, size) :<| Seq.Empty
fill ((bId, file, size) :<| (mid :|> (bId', file', _)))
  | size == 0 = (bId, file, size) :<| fill (mid :|> (bId', file', 0))
  | otherwise = 
    if size > file' then
      (bId, file, 0) :<| fill ((bId', file', size - file') :<| mid)
    else if size == file' then
      (bId, file, 0) :<| (bId', file', 0) :<| fill mid
    else -- size < file'
      (bId, file, 0) :<| (bId', size, 0) :<| fill (mid :|> (bId', file' - size, 0))

expand :: Seq (Int, Int, Int) -> [Integer]
expand Seq.Empty = []
expand ((bId, file, size) :<| rest) = 
  replicate file (fromIntegral bId) ++ replicate size 0 ++ expand rest

checksum :: Seq (Int, Int, Int) -> Integer
checksum = sum . zipWith (*) [0..] . expand

fill2 :: Int -> Seq (Int, Int, Int) -> Seq (Int, Int, Int)
fill2 max_id Seq.Empty = Seq.Empty 
fill2 max_id s@(Seq.Empty :|> _) = s
fill2 max_id s@(rest :|> (bId, file, size) :|> (bId', file', size')) = 
  -- traceShow (expand s) $
  if bId' >= max_id then
    fill2 max_id (rest :|> (bId, file, size)) :|> (bId', file', size')
  else
    case fit (bId', file', size') rest of
      Nothing    -> 
        if file' <= size then
          -- traceShow ((bId, file, size), (bId', file', size'), (bId, file, 0), (bId', file', size' + size)) $
          fill2 bId' $ (rest :|> (bId, file, 0)) :|> (bId', file', size' + size)
        else
          fill2 bId' (rest :|> (bId, file, size)) :|> (bId', file', size')
      Just rest' -> 
        fill2 bId' $ rest' :|> (bId, file, size + file' + size')

fit :: (Int, Int, Int) -> Seq (Int, Int, Int) -> Maybe (Seq (Int, Int, Int))
fit (bId, file, size) Seq.Empty = Nothing
fit (bId, file, size) ((bId', file', size') :<| rest) = 
  if file <= size' then
    Just $ (bId', file', 0) :<| (bId, file, size' - file) :<| rest
  else
    ((bId', file', size') :<|) <$> fit (bId, file, size) rest

-- 6307279963620
-- 6307279963620 correct
-- 6307231144130 too low
main :: IO ()
main = do -- quickCheck prop 
  input <- parse <$> readFile "input/09.txt"
  -- let input = parse "939058747"
  -- -- let input = parse "2333133121414131402"
  -- let input = parse "805264378167519"
  -- let input = parse "805264305281689"

  -- print $ expand input
  -- print $ expand $ fill2 maxBound input
  putStr "Part 1: "
  print $ checksum $ fill input

  putStr "Part 2: "
  print $ checksum $ fill2 maxBound input

  -- traverse_ print $ fill2 maxBound input

-- parse2 :: String -> [(Int, Int)]
-- parse2 inp = do
--   line <- lines inp
--   case words line of
--     [".", len] -> return (-1 , read len)
--     [bid, len] -> return (read bid, read len)
--     _          -> error $ "parse error: " ++ line

format :: Seq (Int, Int, Int) -> [(Int, Int)]
format Seq.Empty = []
format ((bid, file, free) :<| rest) = 
  if free == 0 then
    (bid, file) : format rest
  else
    (bid, file) : (-1, free) : format rest

prop :: QuickCheck.Property
prop = 
  let 
    gen_digits :: QuickCheck.Gen [(Int, Int)]
    gen_digits = QuickCheck.listOf1 $ do 
      file <- QuickCheck.chooseInt (1, 9)
      free <- QuickCheck.chooseInt (0, 9)
      return (file, free)
  in
    QuickCheck.forAll gen_digits $ \digits -> 
      monadicIO $ do    
        -- -- monitor $ QuickCheck.collect (length digits)
        let input = map intToDigit $ tail $ concat [ [file, free] | (file, free) <- digits ]
        let output = checksum $ fill2 maxBound $ parse input

        (_, stdout, _) <- run (readProcessWithExitCode "nix" ["run", "nixpkgs#nodejs_22", "test.js"] input)
        -- let ref_pairs = parse2 stdout
        -- monitor $ QuickCheck.collect (length ref_pairs)
        return $ output QuickCheck.=== read stdout

