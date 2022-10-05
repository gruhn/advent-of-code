module Main where

import Interpreter (parser, run, State (getMemory))
import Utils (parseHardError)
import qualified Data.Map as M

main :: IO ()
main = do
  program <- parseHardError parser <$> readFile "2016/input/12.txt"

  putStr "Part 1: "
  print $ getMemory $ run program mempty

  putStr "Part 2: "
  print $ getMemory $ run program (M.singleton 'c' 1)