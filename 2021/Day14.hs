-- stack runghc --resolver lts-18.18
module Day14 where
import Text.Parsec
import Text.Parsec.String
import qualified Data.Map as Map
import Data.List (group, sort)

type Rule = (Char, Char, Char)

puzzleInput :: Parser (String, [Rule])
puzzleInput = do
    template <- many1 upper
    _ <- newline
    _ <- newline
    rules <- rule `sepBy` newline
    return (template, rules)

rule :: Parser Rule
rule = do
    [a,c] <- count 2 upper
    _ <- string " -> "
    [b] <- count 1 upper
    return (a,b,c)

main :: IO ()
main = do
    inputRaw <- readFile "14-input.txt"
    let parsed = parse (puzzleInput <* eof) "" inputRaw
    let results = do
            (template, rules) <- parsed
            return $ iterate (step rules) (initHist template)
        score hist =
            let counts = map snd . Map.toList . snd $ hist
            in (maximum counts - minimum counts)

    putStr "Part 1: "
    print $ fmap (score . (!! 10)) results
    putStr "Part 2: "
    print $ fmap (score . (!! 40)) results

countOccurs :: String -> [(Char, Int)]
countOccurs = map pivotCount . group . sort
    where
        pivotCount :: String -> (Char, Int)
        pivotCount (c:cs) = (c, length cs + 1)

type Histogram = (Map.Map (Char, Char) Int, Map.Map Char Int)

initHist :: String -> Histogram
initHist template =
    let histPairs = Map.fromList 
            $ zip (zip template (tail template)) (repeat 1)
        histSymbols = Map.fromList 
            $ countOccurs template
    in (histPairs, histSymbols)

applyRule :: Histogram -> Rule -> Histogram
applyRule (histPairs, _) (a,b,c) =
    let count = Map.findWithDefault 0 (a,c) histPairs
        histPairs' = Map.fromList [ ((a,b), count), ((b,c), count) ]
        histSymbols' = Map.fromList [ (b, count) ]
    in (histPairs', histSymbols')

mergeHist :: Histogram -> Histogram -> Histogram
mergeHist (hp1, hs1) (hp2, hs2) =
    (Map.unionWith (+) hp1 hp2, Map.unionWith (+) hs1 hs2)

step :: [Rule] -> Histogram -> Histogram
step rules hist = 
    foldr (mergeHist . applyRule hist) (Map.empty, snd hist) rules