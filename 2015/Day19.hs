
module Main where
import Text.Megaparsec
import Text.Megaparsec.Char ( letterChar, newline, string )
import Data.Void (Void)
import Data.List (stripPrefix, mapAccumR, intercalate, isSuffixOf, isPrefixOf)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Function ((&))
import Data.Maybe (mapMaybe, isJust, fromJust)
import Control.Applicative (Applicative(liftA2), Alternative ((<|>), many))
import Data.Tuple (swap)
import Control.Monad (void)
import Data.Functor ((<&>))
import Control.Arrow (second)
import Utils (distinct, splitOn)
import Data.Foldable (find)

data RuleBody n = Term String | NonTerm String n (RuleBody n) 
    deriving Show

instance Functor RuleBody where
  fmap f (Term t) = Term t
  fmap f (NonTerm t n rest) = 
      NonTerm t (f n) (fmap f rest)

instance Foldable RuleBody where
    foldr f b (Term t) = b
    foldr f b (NonTerm t n rest) = f n (foldr f b rest)

instance Traversable RuleBody where
    sequenceA (Term t) = pure (Term t)
    sequenceA (NonTerm t fa rest) = 
        NonTerm t <$> fa <*> sequenceA rest

type Grammar = Map.Map String [RuleBody String]

showRuleBody :: RuleBody String -> String
showRuleBody (Term t) = t
showRuleBody (NonTerm t n rest) = 
    t ++ n ++ showRuleBody rest

-- prop> \(nonTerms :: String, str :: String) -> showRuleBody (ruleBodyFrom (pure <$> nonTerms) str) == str
-- +++ OK, passed 100 tests.

ruleBodyFrom :: [String] -> String -> RuleBody String
ruleBodyFrom nonTerminals word = go word []
    where
        nonTerminals' = reverse <$> nonTerminals

        go :: String -> String -> RuleBody String
        go [] [] = Term ""
        go [] pre = Term (reverse pre)
        go (c:cs) pre =
            case find (`isPrefixOf` (c:pre)) nonTerminals' of
                Nothing -> go cs (c:pre)
                Just nt' -> 
                    let suf' = fromJust (stripPrefix nt' (c:pre)) 
                    in  NonTerm (reverse suf') (reverse nt') (go cs [])

grammarFrom :: [(String, String)] -> Grammar
grammarFrom arrowPairs = 
    let non_terminals = fst <$> arrowPairs

        ruleFrom (rhs, lhs) = Map.singleton rhs [ruleBodyFrom non_terminals lhs]

    in  Map.unionsWith (++) (ruleFrom <$> arrowPairs)

--- >>> commonPrefix "hello world" "help me"
-- "hel"

commonPrefix :: String -> String -> String
commonPrefix [] _ = []
commonPrefix _ [] = []
commonPrefix (a:as) (b:bs)
    | a == b    = a : commonPrefix as bs
    | otherwise = []

data SyntaxTree = Node String [RuleBody SyntaxTree]

syntaxTree :: Grammar -> String -> SyntaxTree
syntaxTree grammar start = 
    let expansions = Map.findWithDefault [] start grammar
    in  Node start ((syntaxTree grammar <$>) <$> expansions)

getLevel :: Int -> SyntaxTree -> [String]
getLevel 0 (Node n _) = [n]
getLevel l (Node _ ruleBodies) = ruleBodies >>= 
    fmap showRuleBody . traverse (getLevel (l-1))

enumerate :: SyntaxTree -> [String]
enumerate tree = [0..] >>= \l -> getLevel l tree 

prune :: String -> SyntaxTree -> SyntaxTree
prune "" (Node rh _) = Node rh []
prune word (Node rh rbs) = Node rh (go <$> filter go' rbs)
    where
        prefix :: RuleBody a -> String
        prefix (Term t) = t
        prefix (NonTerm t _ _) = t

        go :: RuleBody SyntaxTree -> RuleBody SyntaxTree
        go (Term t) = Term t
        go (NonTerm t tree rest) = 
            let restWord = fromJust (stripPrefix t word)
            in  NonTerm t (prune restWord tree) rest

        go' :: RuleBody SyntaxTree -> Bool
        go' rb = prefix rb `isPrefixOf` word

{- 
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

-}

type Parser = Parsec Void String

parser :: Parser (Grammar, String)
parser =
    let rule :: Parser (String, String)
        rule = (,)
            <$> some letterChar
            <*  string " => "
            <*> some letterChar

        rules :: Parser Grammar
        rules = grammarFrom
            <$> rule `sepEndBy` newline

        word :: Parser String
        word = newline *> some letterChar

    in  (,) <$> rules <*> word

main :: IO ()
main = do
    input <- parse parser "" <$> readFile "2015/input/19.txt"
    case input of
        Left err -> putStr (errorBundlePretty err)
        Right (grammar, word) -> do
            putStr "Part 1: "
            let rb' = ruleBodyFrom (Map.keys grammar) word

            print $ sum $ (\n -> grammar Map.! n & length) <$> rb'

            putStr "Part 2: "
            -- print 
            --     $ take 100 
            --     $ enumerate 
            --     $ prune word 
            --     $ syntaxTree grammar "e"

            -- print $ length $ distinct $ replacements rules molecule
            -- let rules' = swap <$> rules
            -- print $ shortestPath rules' molecule "e"

            -- print $ replacements rules molecule
            -- putStr $ drawTree
            --        $ pruneDuplicates
            --        -- $ pruneLength (length molecule)
            --        $ replacementTree rules' molecule