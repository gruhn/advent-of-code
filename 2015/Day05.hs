module Main where
import Text.Megaparsec (Parsec, some, sepBy, parse, errorBundlePretty)
import Data.Void (Void)
import Text.Megaparsec.Char (alphaNumChar, newline)
import Data.List (group, isInfixOf, sort)
import Data.Function ((&))

parser :: Parsec Void String [String]
parser = some alphaNumChar `sepBy` newline

vowelCount :: String -> Int
vowelCount = length . filter (`elem` "aeiou")

stutter :: String -> Bool
stutter = any (>=2) . fmap length . group

containsNeiter :: [String] -> String -> Bool
containsNeiter blacklist str =
    not (any (`isInfixOf` str) blacklist)

-- >>> duplicates "aabcdeb"
-- "ab"

duplicates :: Ord a => [a] -> [a]
duplicates = 
    fmap head . filter ((>=2) . length) . group . sort

-- >>> isolatedPairs "qjhvhtzxzqqjkmpb" & duplicates
-- ["qj"]

isolatedPairs :: String -> [String]
isolatedPairs str =
    let str' = concat . filter ((<=2) . length) . group $ str
        list x y = [x,y]
        pairs = zipWith list str' (tail str')
    in  pairs

-- >>> symmetricTriples "qjhvhtzxzqqjkmpb"
-- ["hvh","zxz"]

symmetricTriples :: String -> [String]
symmetricTriples (c1:c2:c3:cs)
    | c1 == c3 = [c1,c2,c3] : symmetricTriples (c2:c3:cs)
    | otherwise = symmetricTriples (c2:c3:cs)
symmetricTriples _ = []

isNice1 :: String -> Bool
isNice1 str = stutter str
    && vowelCount str >= 3
    && containsNeiter [ "ab", "cd", "pq", "xy"] str

-- >>> isNice2 "qjhvhtzxzqqjkmpb" == True
-- >>> isNice2 "haegwjzuvuyypxyu" == False
-- True
-- True

isNice2 :: String -> Bool
isNice2 str = 
    (not . null . duplicates . isolatedPairs $ str)
    && (not . null . symmetricTriples $ str)

main :: IO ()
main = do
    input <- parse parser "" <$> readFile "2015/input/05.txt"
    case input of
        Left error -> putStr (errorBundlePretty error)
        Right lines -> do
            putStr "Part 1: "
            print $ length $ filter isNice1 lines

            putStr "Part 2: "
            print $ length $ filter isNice2 lines
