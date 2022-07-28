module Main where
import Text.Megaparsec (Parsec, some, sepBy, parse, errorBundlePretty)
import Data.Void (Void)
import Text.Megaparsec.Char (letterChar, string, hspace, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import qualified Data.Map as Map
import Data.List (findIndex, elemIndex)

type Pattern = Map.Map String Int

type Parser = Parsec Void String

parser :: Parser [Pattern]
parser =
    let feature :: Parser (String, Int)
        feature = (,) 
            <$> some letterChar 
            <*  string ": " 
            <*> decimal 

        line :: Parser Pattern
        line = Map.fromList <$> do
            string "Sue " 
            decimal 
            string ": " 
            feature `sepBy` string ", "

    in  line `sepBy` newline

submap :: Eq a => [a] -> Map.Map a b -> Map.Map a b
submap keys =
    Map.filterWithKey (\key _ -> key `elem` keys)

main :: IO ()
main = do 
    input <- parse parser "" <$> readFile "2015/input/16.txt"
    case input of
        Left err -> putStr (errorBundlePretty err)
        Right input -> do
            let superPattern = Map.fromList
                    [ ("children", 3), ("cats",  7)
                    , ("samoyeds", 2), ("pomeranians", 3)
                    , ("akitas", 0), ("vizslas", 0)
                    , ("goldfish", 5), ("trees", 3)
                    , ("cars", 2), ("perfumes", 1)
                    ]

            putStr "Part 1: "
            print $ (+) 1 <$> findIndex (`Map.isSubmapOf` superPattern) input

            putStr "Part 2: "
            let split pattern = 
                    let greater = submap ["cats", "trees"] pattern
                        fewer   = submap ["pomeranians", "goldfish"] pattern
                        equal   = (pattern Map.\\ greater) Map.\\ fewer
                    in  (greater, fewer, equal)

                isSubPattern pattern =
                    let (gt1, lt1, eq1) = split pattern
                        (gt2, lt2, eq2) = split superPattern
                    in  Map.isSubmapOfBy (>) gt1 gt2
                     && Map.isSubmapOfBy (<) lt1 lt2  
                     && Map.isSubmapOf eq1 eq2

            print $ (+) 1 <$> findIndex isSubPattern input