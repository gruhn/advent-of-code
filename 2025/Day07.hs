module Main (main) where
import Data.IntSet (IntSet)
import Data.List (elemIndex, uncons, mapAccumL)
import qualified Data.IntSet as IntSet
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

type SplitterRow = IntSet

parseSplitterRow :: String -> SplitterRow
parseSplitterRow line = IntSet.fromList $ do
  (x, '^') <- zip [0..] line
  return x

parse :: String -> Maybe (Int, [SplitterRow])
parse input = do
  (first_line, rest_lines) <- uncons $ lines input
  start_x <- elemIndex 'S' first_line
  let splitter_rows = map parseSplitterRow rest_lines
  return (start_x, splitter_rows)

type BeamRow = IntMap Int

step :: BeamRow -> SplitterRow -> (BeamRow, SplitterRow)
step beams splitter_row =
  let
    missed_splitters :: SplitterRow
    missed_splitters = splitter_row IntSet.\\ IntMap.keysSet beams

    new_beams :: BeamRow
    new_beams = IntMap.fromListWith (+) $ do
      (x, count) <- IntMap.toList beams
      if IntSet.member x splitter_row then
        [(x-1, count), (x+1, count)]
      else
        [(x, count)]
  in
    (new_beams, missed_splitters)

main :: IO ()
main = do
  Just (start_x, splitter_rows) <- parse <$> readFile "input/07.txt"
  let initial_beams = IntMap.singleton start_x 1
  let (final_beams, missed_splitters) = mapAccumL step initial_beams splitter_rows

  putStr "Part 1: "
  let total_splitter_count = sum $ map IntSet.size splitter_rows
  let missed_splitter_count = sum $ map IntSet.size missed_splitters
  print $ total_splitter_count - missed_splitter_count

  putStr "Part 2: "
  print $ sum $ IntMap.elems final_beams
