module Main where

import IntcodeComputer
import Text.Megaparsec (errorBundlePretty, parse)

main :: IO ()
main = do
    input <- parse parser "" <$> readFile "2019/input/09.txt"
    case input of
        Left error -> putStr (errorBundlePretty error)
        Right program -> do
            let testP1 = programFrom [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
                testP2 = programFrom [1102,34915192,34915192,7,4,7,99,0]
                testP3 = programFrom [104,1125899906842624,99]
                testP4 = programFrom [203,6,99]
                
            print $ run testP1 []
            print $ run testP2 []
            print $ run testP3 []
            print $ run testP4 [1]

            putStr "Part 1: "
            print $ run program [1]

            putStr "Part 2: "