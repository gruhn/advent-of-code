module Main where
import Data.SBV (Goal, OrdSymbolic ((.>=), smax), constrain, sInteger, maximize, optimize, OptimizeStyle (Lexicographic), EqSymbolic ((.==)), Symbolic, SInteger)
import Data.List (transpose)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Documentation.SBV.Examples.Misc.SetAlgebra (SI)
import Data.Function ((&))

ingredients :: Num a => [([a], String)]
ingredients = 
    [ ([3, 0, 0, -3, 2], "sugar")
    , ([-3, 3, 0, 0, 9], "sprinkles")
    , ([-1, 0, 4, 0, 1], "candy")
    , ([0, 0, -2, 2, 8], "chocolate")
    ]

scalarProduct :: Num a => [a] -> [a] -> a
scalarProduct as1 as2 = 
    sum (zipWith (*) as1 as2)

amounts :: Symbolic [SInteger]
amounts = do
    vars <- mapM sInteger (snd <$> ingredients)
    for_ vars (\v -> constrain $ v .>= 0)
    constrain $ sum vars .== 100
    return vars

features :: [SInteger] -> [SInteger]
features vars = ingredients
    <&> fst & transpose 
    <&> scalarProduct vars
    <&> smax 0  

goalPart1 :: Goal
goalPart1 = do
    vars <- amounts
    let feats = init (features vars)
    maximize "goal" $ product feats

goalPart2 :: Goal
goalPart2 = do
    vars <- amounts
    let (calories:feats) = reverse $ features vars
    constrain $ calories .== 500
    maximize "goal" $ product feats

main :: IO ()
main = do
    putStrLn "Part 1: "
    print =<< optimize Lexicographic goalPart1

    putStrLn "Part 2: "
    print =<< optimize Lexicographic goalPart2