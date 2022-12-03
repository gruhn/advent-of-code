module Main where
import Utils (parseHardError)
import qualified Data.Map as M
import Interpreter (parser, run, State (getMemory))

main :: IO ()
main = do
  program <- parseHardError parser <$> readFile "input/23_manually_optimized.txt"

  let get_result = (M.! 'a') . getMemory . last

  putStr "Part 1: "
  print $ get_result $ run program (M.singleton 'a' 7)

  putStr "Part 2: "
  print $ get_result $ run program (M.singleton 'a' 12)