{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.SBV (Goal, sChar_, SChar, SString, EqSymbolic (sNotElem), constrain, OrdSymbolic ((.<)), sChars, sString, SymVal (literal), minimize, Metric (toMetricSpace, fromMetricSpace), SInteger)
import Data.Functor ((<&>))
import Data.String (fromString)
import qualified Data.SBV.Char as SChar

-- instance Metric String where
--     toMetricSpace str = undefined 
--     fromMetricSpace str = undefined 

goal :: String -> Goal
goal oldPass = do
    nextPass <- sString "password"

    -- passwords may not contain the letters i, o, or l
    constrain $ literal 'i' `SChar.notElem` nextPass
    constrain $ literal 'l' `SChar.notElem` nextPass
    constrain $ literal 'o' `SChar.notElem` nextPass

    -- new password must "come after" old password
    constrain $ literal oldPass .< nextPass

    -- passwords must include one increasing straight of at least three letters

    -- passwords must contain at least two different, non-overlapping pairs
    
    -- minimize "goal" nextPass

main :: IO ()
main = do
    let oldPass = "vzbxkghb"

    putStrLn "Part 1: "