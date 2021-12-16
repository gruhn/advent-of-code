
#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

module Day07 where
import ParseUtil (splitOn)
import Data.List ((\\), find, delete, findIndex, elemIndex, sort, intersperse, intersect)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

parseLine :: String -> Maybe ([String], [String])
parseLine line = 
    case map words $ splitOn "|" line of
        [input, output] -> Just (input, output)
        _               -> Nothing

parse :: String -> Maybe [([String], [String])]
parse = mapM parseLine . lines

main :: IO ()
main = do
    input <- readFile "08-input2.txt"
    let decoded = decode input
    putStr "Part 1: "
    print $ length . filter (`elem` [1,4,7,8]) . concat <$> decoded
    putStr "Part 2: "
    print $ sum . map digitsToNumber <$> decoded

decode :: String -> Maybe [[Int]]
decode input = do
    parsed <- parse input
    mapM decodeSignals parsed

deduceMapping :: [String] -> Map.Map Char Char
deduceMapping signals = 
    let segmentCountOf n = filter ((==n) . length) signals
        notIn cs = map ("abcdefg" \\) cs
        intersection css = foldr intersect "abcdefg" (concat css)
        
        one = segmentCountOf 2
        seven = segmentCountOf 3
        four = segmentCountOf 4
        twoThreeFive = segmentCountOf 5
        zeroSixNine = segmentCountOf 6
        eight = segmentCountOf 7

        a = intersection [zeroSixNine, twoThreeFive, seven] 
        f = intersection [zeroSixNine, one, four, seven] 
        g = intersection [zeroSixNine, twoThreeFive, notIn seven]

        b = intersection [zeroSixNine, four, notIn seven] \\ g
        c = intersection [one, four, seven] \\ f 
        d = intersection [twoThreeFive, four, notIn seven] \\ g

        e = "abcdefg" \\ concat [a,b,c,d,f,g]

        pairs = zip (map head [a,b,c,d,e,f,g]) "abcdefg"
    in Map.fromList pairs

decodeDigit :: Map.Map Char Char -> String -> Maybe Int
decodeDigit mapping signal = do
    decodedSignal <- sort <$> mapM (`Map.lookup` mapping) signal
    elemIndex decodedSignal [ "abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg" ]

digitsToNumber :: [Int] -> Int
digitsToNumber [] = 0
digitsToNumber (x:xs) = 
    let n = length xs 
    in  x * 10^n + digitsToNumber xs

decodeSignals :: ([String], [String]) -> Maybe [Int]
decodeSignals (inputSignals, outputSignals) = do
    let mapping = deduceMapping inputSignals
    mapM (decodeDigit mapping) outputSignals