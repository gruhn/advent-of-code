module Main where

import IntcodeComputer
import Text.Megaparsec (errorBundlePretty, parse)
import Lens.Micro.Extras (view)

{-
    
>>> testP1 = programFrom [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
>>> run testP1 []
[99,0,101,1006,101,16,100,1008,100,1,100,1001,-1,204,1,109]

>>> testP2 = programFrom [1102,34915192,34915192,7,4,7,99,0]
>>> run testP2 []
[1219070632396864]

>>> testP3 = programFrom [104,1125899906842624,99]
>>> run testP3 []
[1125899906842624]

-}

main :: IO ()
main = do
    input <- parse parser "" <$> readFile "2019/input/09.txt"
    case input of
        Left error -> putStr (errorBundlePretty error)
        Right program -> do
            putStr "Part 1: "
            print $ run program [1]

            putStr "Part 2: "
            print $ run program [2]
