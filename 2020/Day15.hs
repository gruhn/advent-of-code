module Main where

import qualified Data.Map as Map
import Data.Map (Map)

turn :: Int -> Map Int Int -> Int -> [Int]
turn i nums prev = 
    let next = i - Map.findWithDefault i prev nums
        nums' = Map.insert prev i nums
    in next : (turn (i+1) nums' $! next)

main :: IO ()
main = do
    let start = [0,20,7,16,1,18,15]
        startMap = Map.fromList $ zip start [1..] 
        result = start ++ turn (length start) startMap (last start)

    print (result !! (2020-1))
    print (result !! (30000000-1))