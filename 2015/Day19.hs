module Main where
import Text.Megaparsec (Parsec, some, sepBy, parse, errorBundlePretty, sepEndBy, choice, manyTill, MonadParsec (eof, try, lookAhead), someTill, (<?>), takeRest, option, anySingle)
import Data.Void (Void)
import Text.Megaparsec.Char (hspace, letterChar, string, newline)
import Data.List (stripPrefix, mapAccumR)
import qualified Data.Set as Set
import Data.Tree (Tree (Node, rootLabel), unfoldTree, drawTree)
import Data.Function ((&))
import Data.Maybe (mapMaybe)
import Control.Applicative (Applicative(liftA2), Alternative ((<|>), many))
import Data.Tuple (swap)
import Control.Monad (void)
import Data.Functor ((<&>))

distinct :: Ord a => [a] -> [a]
distinct = Set.toList . Set.fromList

type Parser = Parsec Void String

type Rule = (String, String)

parser :: Parser ([Rule], String)
parser =
    let rule :: Parser Rule
        rule = (,)
            <$> some letterChar 
            <*  string " => " 
            <*> some letterChar 
        
        rules :: Parser [Rule]
        rules = rule `sepEndBy` newline

        molecule :: Parser String
        molecule = newline *> some letterChar

    in  (,) <$> rules <*> molecule

replacementsWith :: Rule -> String -> [String]
replacementsWith (match,replace) [] = []
replacementsWith (match,replace) (c:cs) = 
    case stripPrefix match (c:cs) of
        Nothing   -> (c:) <$> replacementsWith (match,replace) cs
        Just rest -> 
            let rep  = replace ++ rest
                reps = (c:) <$> replacementsWith (match,replace) cs 
            in  rep : reps

replacements :: [Rule] -> String -> [String]
replacements rules molecule =
    concatMap (`replacementsWith` molecule) rules

replacementTree :: [Rule] -> String -> Tree String
replacementTree rules = 
    unfoldTree (\str -> (str, replacements rules str))

pruneDepth :: Int -> Tree String -> Tree String
pruneDepth 0 (Node str _) = Node str []
pruneDepth d (Node str children) = 
    Node str (pruneDepth (d-1) <$> children)

-- pruneLength :: Int -> Tree String -> Tree String
-- pruneLength l (Node str children) =
--     let children' = filter ((l >=) . length . rootLabel) children
--     in  Node str (pruneLength l <$> children')

pruneDuplicates :: Tree String -> Tree String
pruneDuplicates =
    let go :: Set.Set String -> Tree String -> (Set.Set String, Tree String)
        go seen0 (Node str children0) =
            let seen1 = Set.insert str seen0
                notSeen = (`Set.notMember` seen1) . rootLabel

                children1 = filter notSeen children0
                (seen2, children2) = mapAccumR go seen1 children1

            in  (seen2, Node str children2)
    in  snd . go Set.empty

shortestPath :: [Rule] -> String -> String -> Maybe Int
shortestPath rules start end =
    let searchSpace = replacementTree rules start
            -- & pruneLength (length end)
            & pruneDuplicates

        go :: Int -> Tree String -> Maybe Int
        go depth (Node str children)
            | str == end = Just depth
            | otherwise  = 
                let minJust :: Tree String -> Maybe Int -> Maybe Int
                    minJust child Nothing = go (depth+1) child
                    minJust child (Just bestDepth) =
                        case go (depth+1) (pruneDepth (bestDepth-1) child) of
                            Nothing -> Just bestDepth
                            Just bestDepth' -> Just (min bestDepth bestDepth')
                in  foldr minJust Nothing children

    in  go 0 searchSpace

data Match = Filler String | Match String
    deriving Show

splitOnAny :: [String] -> String -> [Match]
splitOnAny splitStrings str =
    let match  = Match  <$> choice (string <$> splitStrings)
        filler = Filler <$> anySingle `manyTill` lookAhead match

        substrings :: Parser [Match]
        substrings = do
            front <- many (try (match <|> filler))
            rest  <- takeRest
            case rest of
                [] -> return front
                rs -> return (front ++ [Filler rest])

    in  case parse substrings "" str of
            Left err -> error (errorBundlePretty err)
            Right result -> result

rewriteTree :: [Rule] -> Match -> Tree String
rewriteTree rules start =
    let build (Filler str) = (str, [])
        build (Match str)  = (str, splitOnAny (rules <&> fst & distinct) str)

        tree0 = unfoldTree build start

    in  tree0

main :: IO ()
main = do
    input <- parse parser "" <$> readFile "2015/input/19.txt"
    case input of
        Left err -> putStr (errorBundlePretty err)
        Right (rules,molecule) -> do
            -- putStr "Part 1: "
            -- print $ length $ distinct $ replacements rules molecule

            putStr "Part 2: "
            let rules' = swap <$> rules
            print $ shortestPath rules' molecule "e"

            -- print $ replacements rules molecule
            -- putStr $ drawTree
            --        $ pruneDuplicates
            --        -- $ pruneLength (length molecule)
            --        $ replacementTree rules' molecule
