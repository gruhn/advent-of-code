module Main where
import Utils (parseHardError, takeUntil)
import Interpreter (parser, State (getOutput, getPointer, getMemory), run, Program, Memory)
import qualified Data.Map as M
import Data.Foldable (for_)
import Data.Maybe (mapMaybe)
import qualified Data.Set as S

main :: IO ()
main = do
  program <- parseHardError parser <$> readFile "2016/input/25.txt"

  putStrLn "Part 1: "
  let checks = fmap (\inp -> (inp, checkWithInput program inp)) [0..]
  for_ (takeUntil snd checks) print

checkWithInput :: Program -> Int -> Bool
checkWithInput program a = and $ zipWith (&&) correct_output is_looping
  where
    states = run program (M.singleton 'a' a)

    correct_output :: [Bool]
    correct_output = go 0 states
      where
        go :: Int -> [State] -> [Bool]
        go out [] = [False]
        go out (state:states) = 
          case getOutput state of
            Nothing   -> True : go out states
            Just out' -> (out == out') : go (1 - out) states

    is_looping :: [Bool]
    is_looping = go S.empty states
      where
        go :: S.Set (Memory, Int) -> [State] -> [Bool]
        go seen [] = [False]
        go seen (state:states)
          | S.member (mem, i) seen = []
          | otherwise              = True : go seen' states
          where
            mem = getMemory state 
            i = getPointer state
            seen' = S.insert (mem, i) seen