{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.SBV (Goal, sChar_, SChar, SString, EqSymbolic (sNotElem, (.==)), constrain, OrdSymbolic ((.<), (.>), (.>=)), sChars, sString, SymVal (literal), minimize, Metric (toMetricSpace, fromMetricSpace), SInteger, sChar, optimize, OptimizeStyle (Lexicographic), sat)
import Data.Functor ((<&>))
import Data.String (fromString)
import qualified Data.SBV.Char as SChar
import qualified Data.SBV.String as SString
import Data.List ( (\\) )
import Data.Foldable (for_)

goal :: String -> Goal
goal oldPass = do
    nextPass <- sString "password"

    -- passwords must be exactly eight lowercase letters
    -- passwords may not contain the letters i, o, or l
    let alphabet = literal $ ['a' .. 'z'] \\ "iol"
    constrain $ SString.length nextPass .== 8
    for_ [0,1,2,3,4,5,6,7] $ \i ->
        constrain $ (nextPass SString.!! i) `SChar.elem` alphabet

    -- new password must "come after" old password
    constrain $ literal oldPass .< nextPass
    constrain $ nextPass .< literal ""

    -- passwords must include one increasing straight of at least three letters
    charS1 <- sChar "straight char 1"
    charS2 <- sChar "straight char 2"
    charS3 <- sChar "straight char 3"
    let straight = SString.implode [charS1,charS2,charS3]
    constrain $ SChar.ord charS1 +1 .== SChar.ord charS2
    constrain $ SChar.ord charS2 +1 .== SChar.ord charS3
    constrain $ straight `SString.isInfixOf` nextPass

    -- passwords must contain at least two different, non-overlapping pairs
    charP1 <- sChar "first pair character"
    charP2 <- sChar "second pair character"

    let pair1 = SString.implode [charP1,charP1]
        pair2 = SString.implode [charP2,charP2]

        indexPair1 = SString.offsetIndexOf nextPass pair1 0
        indexPair2 = SString.offsetIndexOf nextPass pair2 (indexPair1+2)

    constrain $ indexPair1 .> -1
    constrain $ indexPair2 .> -1
    
    -- minimize "goal" (SString.strToNat nextPass)

main :: IO ()
main = do
    let oldPass = "vzbxkghb"
    putStrLn "Part 1: "

    let oldPass' = "vzbxxyzz"
    print =<< sat (goal oldPass')