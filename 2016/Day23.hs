module Main where
import Utils (parseHardError)
import qualified Data.Map as M
import Interpreter (parser, run, eval, State (getMemory, State, getPointer))
import Data.Foldable (for_)
import Data.Function ((&))

main :: IO ()
main = do
  program <- parseHardError parser <$> readFile "2016/input/23.txt"

  print program

  putStr "Part 1: "
  print $ last $ run program (M.singleton 'a' 7)

  putStr "Part 2: "
  let f s = getPointer s == 16 && (getMemory s M.! 'c' <= length program)
  print $ take 10 
    $ filter f
    $ run program (M.singleton 'a' 12)