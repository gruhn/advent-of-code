module Main where

import Interpreter (parser, run, State (getMemory))
import Utils (parseHardError)
import qualified Data.Map as M

main :: IO ()
main = do
  program <- parseHardError parser <$> readFile "input/12.txt"

  putStr "Part 1: "
  print $ getMemory $ last $ run program mempty

  putStr "Part 2: "
  print $ getMemory $ last $ run program (M.singleton 'c' 1)