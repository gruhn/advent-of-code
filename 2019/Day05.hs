module Main where

import IntcodeComputer
import Text.Megaparsec (parse, errorBundlePretty)

main :: IO ()
main = do
    input <- parse parser "" <$> readFile "2019/input/05.txt"
    case input of
        Left error -> putStr (errorBundlePretty error)
        Right program -> do
            putStr "Part 2: "
            print $ run program [5]