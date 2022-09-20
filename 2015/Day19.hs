module Main where
import Utils (distinct, Parser, parseHardError, converge)
import Text.Megaparsec (Parsec, sepEndBy, some, many)
import Text.Megaparsec.Char ( letterChar, newline, string, lowerChar )
import Data.Array ( listArray, (!) )
import Data.Char ( isUpper, isLower )
import qualified Data.Map as M
import Control.Monad (guard)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import Control.Arrow (second)

type Grammar = Map.Map String [[String]]

parser :: Parser (Grammar, [String])
parser =
    let token :: Parser String
        token = (:) <$> letterChar <*> many lowerChar

        rule :: Parser (String, [String])
        rule = (,) <$>
            token <* string " => " <*> some token

        grammarFrom :: [(String, [String])] -> Grammar
        grammarFrom = Map.fromListWith (++) . fmap (second pure)

        grammar :: Parser Grammar
        grammar = grammarFrom <$>
            rule `sepEndBy` newline

        word :: Parser [String]
        word = newline *> some token

    in  (,) <$> grammar <*> word

main :: IO ()
main = do
    (grammar, target_word) <- parseHardError parser <$> readFile "2015/input/19.txt"

    -- let rule_heads = distinct $ fst <$> rules
    --     target_word_tokenized = parseHardError (tokenizer rule_heads) target_word
    --     rules_tokenized = second (parseHardError (tokenizer rule_heads)) <$> rules
    --     grammar :: Grammar
    --     grammar = Map.unionsWith (++) . fmap singleton $ rules_tokenized
    --         where
    --             singleton (rh, rb) = Map.singleton rh [rb]

    putStr "Part 1: "
    -- print $ singleReplacementCount grammar target_word_tokenized

    putStr "Part 2: "
    let rules = do
            (rule_head, rule_bodies) <- Map.toList grammar
            rule_body <- rule_bodies
            return (rule_head, rule_body)

    print $ earley rules "e" target_word
        -- tree <- last $ earley rules "e" target_word
        -- case tree of
        --     (Node (rule_head, [], 0) sub_trees) -> [stateCount tree]
        --     _ -> []

type Rule = (String, [String])

type EarleyState = (String, [String], Int)
type EarleyState' = (EarleyState, [EarleyState])

data SyntaxTree = Node EarleyState [SyntaxTree]
    deriving (Ord, Show)

instance Eq SyntaxTree where
    Node state _ == Node state' _ = state == state'

stateCount :: SyntaxTree -> Int
stateCount (Node _ sub_trees) = 1 + sum (fmap stateCount sub_trees)

earley :: [Rule] -> String -> [String] -> [[SyntaxTree]]
earley rules start_token target_word = state_list'
  where
    initial_trees :: [SyntaxTree]
    initial_trees = do
        (rule_head, rule_body) <- rules
        guard (rule_head == start_token)
        return $ Node (rule_head, rule_body, 0) []

    complete_and_predict :: Int -> SyntaxTree -> [SyntaxTree]
    complete_and_predict k tree@(Node (rule_head, [], j) sub_trees) = do
        -- completion
        Node (rule_head', next_token' : rest_tokens', i) sub_trees' <- state_array' ! j
        guard (next_token' == rule_head)
        return $ Node (rule_head', rest_tokens', i) (tree : sub_trees')
    complete_and_predict k tree@(Node (_, next_token : _, j) _) = do
        -- prediction
        (rule_head', rule_body') <- rules
        guard (next_token == rule_head')
        return $ Node (rule_head', rule_body', k) []

    complete_with :: Int -> [SyntaxTree] -> [SyntaxTree]
    complete_with k start = snd <$> rs
      where
        -- xs :: [[M.Map a b]]
        x0 = M.fromListWith min $ zip (fmap stateCount start) start
        xs = x0 : step x0 xs
        rs = M.assocs $ foldl (M.unionWith min) M.empty xs

        f' (_, tree) = zip (stateCount <$> complete_and_predict k tree) (complete_and_predict k tree)

        step seen [] = []
        step seen (x : xs) = if M.null x then [] else y' : step seen' xs
          where
            y = M.fromListWith min $ concatMap f' $ M.assocs x
            y' = M.differenceWith (already min) y seen
            seen' = M.unionWith min y seen

    -- remove if y is already best  
    already :: Eq a1 => (a2 -> a1 -> a1) -> a2 -> a1 -> Maybe a2
    already f x y = if f x y == y then Nothing else Just x

    n = length target_word

    state_array' = listArray (0, n) state_list'
    state_list' = s0' : zipWith3 sk' [1 ..] target_word state_list'

    s0'       = complete_with 0 initial_trees
    sk' k z s = complete_with k [ Node (x, ys, j) sub_trees | Node (x, y : ys, j) sub_trees <- s, y == z]