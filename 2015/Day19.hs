{-# LANGUAGE TupleSections #-}
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
import qualified Data.IntMap as IntMap
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

    print $ run rules "e" target_word
    -- print $ earley' grammar "e" target_word

type EarleyState = (String, [String], Int)

earley' :: Grammar -> String -> [String] -> (Int, IntMap [EarleyState])
earley' grammar start_word target_word = go (0, IntMap.singleton 0 initial_states)
    where
        initial_states = do
            rule_body <- grammar Map.! start_word
            return (start_word, rule_body, 0)

        go :: (Int, IntMap [EarleyState]) -> (Int, IntMap [EarleyState])
        go (k, statesMap)
            | any is_finish states = (k, statesMap)
            | otherwise = go (k+1, scan k $ converge (predict_and_complete k) statesMap)
            where
                states = statesMap IntMap.! k

                is_finish (rule_head, [], 0) = rule_head == start_word
                is_finish _                  = False

        next_element_is :: EarleyState -> String -> Bool
        next_element_is (_, next : _, _) element = next == element
        next_element_is _                _       = False

        predict_and_complete :: Int -> IntMap [EarleyState] -> IntMap [EarleyState]
        predict_and_complete k stateMap = IntMap.insert k (distinct $ states ++ new_states) stateMap
            where
                states = stateMap IntMap.! k

                new_states = do
                    state <- states

                    case state of
                        (rule_head, []         , j) -> -- completion
                            seek <$> filter (`next_element_is` rule_head) states
                                where
                                    seek (rule_head, rule_body, j) = (rule_head, drop 1 rule_body, k)

                        (_        , next : rest, j) -> -- prediction
                            case Map.lookup next grammar of
                                Nothing          -> []
                                Just rule_bodies -> (next, , k) <$> rule_bodies

        scan :: Int -> IntMap [EarleyState] -> IntMap [EarleyState]
        scan k statesMap = IntMap.insert (k+1) new_states statesMap
            where
                next_token = target_word !! k -- TODO

                seek (rule_head, rule_body, j) = (rule_head, drop 1 rule_body, j)

                new_states = do
                    state <- statesMap IntMap.! k
                    guard (state `next_element_is` next_token)
                    return (seek state)

type Rule = (String, [String])

type State = (String , [String], Int)

completeWith :: (Eq b, Ord a) => (b -> b -> b) -> ((a,b) -> [(a,b)]) -> [(a,b)] -> [(a,b)]
completeWith g f start = rs where
  -- xs :: [[M.Map a b]]
  xs = x0 : step x0 xs
  x0 = M.fromListWith g start
  rs = M.assocs $ foldl (M.unionWith g) M.empty xs

  step seen [] = []
  step seen (x:xs) = if M.null x then [] else y' : step seen' xs where
    y     = M.fromListWith g $ concatMap f $ M.assocs x
    y'    = M.differenceWith (already g) y seen
    seen' = M.unionWith g y seen

-- remove if y is already best  
already :: Eq a1 => (a2 -> a1 -> a1) -> a2 -> a1 -> Maybe a2
already f x y = if f x y == y then Nothing else Just x

earley :: [Rule] -> String -> [String] -> [[(State, Int)]]
earley rules start_token target_word = ss where

  start_rules = [((x,ys,0),0) | (x,ys) <- rules, x == start_token]

  n = length target_word

  sa = listArray (0,n) ss
  ss = s0 : zipWith3 sk [1..] target_word ss

  s0       = completeWith min (predComp 0) start_rules
  sk k z s = completeWith min (predComp k)
               [((x,ys,j),c) | ((x,y:ys,j),c) <- s, y == z]

  predComp k ((rule_head, []    , j), c) = do -- completion
    ((x',y':ys',i),d) <- sa ! j
    guard (y' == rule_head)
    return ((x',ys',i),d+c+1)
  predComp k ((_        , next:_, j), c) = do -- prediction
    (x', ys') <- rules
    guard (x' == next)
    return ((x',ys',k),0)

run :: [Rule] -> String -> [String] -> [Int]
run rules startSym zs = [ c+1 |
    ((x,ys,j),c) <- last $ earley rules startSym zs,
      x == startSym, null ys, j == 0
  ]