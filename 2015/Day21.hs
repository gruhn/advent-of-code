module Main where

import Data.SBV
import Data.List (transpose)

weapons :: [[SInteger]]
weapons = 
    [ [  8, 4, 0]
    , [ 10, 5, 0]
    , [ 25, 6, 0]
    , [ 40, 7, 0]
    , [ 74, 8, 0]
    ]

armors :: [[SInteger]]
armors = 
    [ [ 13, 0, 1 ]
    , [ 31, 0, 2 ]
    , [ 53, 0, 3 ]
    , [ 75, 0, 4 ]
    , [ 102, 0, 5 ]
    ]

rings :: [[SInteger]]
rings = 
    [ [ 25, 1, 0 ]
    , [ 50, 2, 0 ]
    , [ 100, 3, 0 ]
    , [ 20, 0, 1 ]
    , [ 40, 0, 2 ]
    , [ 80, 0, 3 ]
    ]

scalarProduct :: Num a => [a] -> [a] -> a
scalarProduct as1 as2 = 
    sum (zipWith (*) as1 as2)

stats :: [[SInteger]]
stats = transpose (weapons ++ armors ++ rings)

var :: String -> Int -> String
var name index =
    name ++ show index

goalPart1 :: Goal
goalPart1 = do 
    ws <- mapM sBool $ var "weapon" <$> [1..length weapons]
    as <- mapM sBool $ var "armor"  <$> [1..length armors]
    rs <- mapM sBool $ var "ring"   <$> [1..length rings]

    let vars = ws ++ as ++ rs

    -- pick exactly one weapon
    constrain $ ws `pbExactly` 1
    -- pick at most one armor
    constrain $ as `pbAtMost` 1
    -- pick at most two rings
    constrain $ rs `pbAtMost` 2

    let bossHitPoints = 103
        bossDamage = 9
        bossArmor = 2

        [costs, damages, armorStats] = stats

        myHitPoints = 100
        myDamage = damages `scalarProduct` (oneIf <$> vars)
        myArmor = armorStats `scalarProduct` (oneIf <$> vars)

        totalCost = costs `scalarProduct` (oneIf <$> vars)

    -- winning condition
    constrain $ bossHitPoints * bossDamage + myHitPoints * bossArmor 
            .<= bossHitPoints * myDamage   + myHitPoints * myArmor

    minimize "goal" totalCost

goalPart2 :: Goal
goalPart2 = do 
    ws <- mapM sBool $ var "weapon" <$> [1..length weapons]
    as <- mapM sBool $ var "armor"  <$> [1..length armors]
    rs <- mapM sBool $ var "ring"   <$> [1..length rings]

    let vars = ws ++ as ++ rs

    -- pick exactly one weapon
    constrain $ ws `pbExactly` 1
    -- pick at most one armor
    constrain $ as `pbAtMost` 1
    -- pick at most two rings
    constrain $ rs `pbAtMost` 2

    let bossHitPoints = 103
        bossDamage = 9
        bossArmor = 2

        [costs, damages, armorStats] = stats

        myHitPoints = 100
        myDamage = damages `scalarProduct` (oneIf <$> vars)
        myArmor = armorStats `scalarProduct` (oneIf <$> vars)

        totalCost = costs `scalarProduct` (oneIf <$> vars)

    -- loosing condition
    constrain $ bossHitPoints * bossDamage + myHitPoints * bossArmor 
             .> bossHitPoints * myDamage   + myHitPoints * myArmor

    maximize "goal" totalCost


main :: IO ()
main = do
    putStrLn "Part 1: "
    print =<< optimize Lexicographic goalPart1

    putStrLn "Part 2: "
    print =<< optimize Lexicographic goalPart2