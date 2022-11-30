module Main where
import Text.Megaparsec (Parsec, sepEndBy, choice, chunk, MonadParsec (lookAhead, try), eof, someTill)
import Text.Megaparsec.Char ( letterChar, newline, string )
import qualified Data.Map as Map
import Data.Function ((&))
import Data.Maybe (catMaybes)
import Control.Applicative ((<|>), many, some)
import Data.Functor ((<&>))
import Utils (distinct, Parser, parseHardError)
import Data.Foldable (find, for_)
import Control.Arrow (second)

type Rule = (String, String)

inputParser :: Parser ([Rule], String)
inputParser =
    let rule :: Parser Rule
        rule = (,) <$>
            some letterChar <* string " => " <*> some letterChar

        rules :: Parser [Rule]
        rules = rule `sepEndBy` newline

        word :: Parser String
        word = newline *> some letterChar

    in  (,) <$> rules <*> word

data Token a = Term String | NonTerm a
    deriving (Show, Eq)

instance Functor Token where
    fmap f (Term    t) = Term t
    fmap f (NonTerm t) = NonTerm (f t)

tokenizer :: [String] -> Parser [Token String]
tokenizer rule_heads =
    let non_term :: Parser (Token String)
        non_term = NonTerm <$> choice (chunk <$> rule_heads)

        term_then_non_term :: Parser (Token String)
        term_then_non_term = Term <$> someTill letterChar (lookAhead non_term)

        term_then_eof :: Parser (Token String)
        term_then_eof = Term <$> some letterChar <* eof

        token = try non_term
            <|> try term_then_non_term
            <|> term_then_eof

    in  many token

untokenize :: [Token String] -> String
untokenize = concatMap unwrap
    where
        unwrap (Term t) = t
        unwrap (NonTerm t) = t

type Grammar = Map.Map String [[Token String]]

singleReplacementCount :: Grammar -> [Token String] -> Int
singleReplacementCount grammar tokens =
    let total_count :: Int
        total_count =
            let replace_options (Term _) = 0
                replace_options (NonTerm t) = length $ grammar Map.! t
            in  sum $ replace_options <$> tokens

        adjacent_non_terms :: [(String, String)]
        adjacent_non_terms =
            let non_term_pair (NonTerm t1) (NonTerm t2) = Just (t1, t2)
                non_term_pair _ _ = Nothing
            in  catMaybes $ zipWith non_term_pair tokens (tail tokens)

        duplicate_count :: Int
        duplicate_count = sum [ 1
            | (rh1, rh2) <- adjacent_non_terms
            , rb1 <- untokenize <$> grammar Map.! rh1
            , rb2 <- untokenize <$> grammar Map.! rh2
            , rh1 ++ rb2 == rb1 ++ rh2
            ]

    in  total_count - duplicate_count

data SyntaxTree = Node String [[Token SyntaxTree]]

syntaxTree :: Grammar -> String -> SyntaxTree
syntaxTree grammar = expand_rule_head
    where
        expand_rule_head :: String -> SyntaxTree
        expand_rule_head rh = Node rh (expand_rule_body <$> grammar Map.! rh)

        expand_rule_body :: [Token String] -> [Token SyntaxTree]
        expand_rule_body = fmap (fmap expand_rule_head)

enumerate :: SyntaxTree -> [String]
enumerate tree = [0..] >>= \level -> go level (NonTerm tree)
    where
        go :: Int -> Token SyntaxTree -> [String]
        go _ (Term t) = [t]
        go 0 (NonTerm (Node t _)) = [t]
        go l (NonTerm (Node _ rbs)) = do
            rb  <- rbs
            rb' <- traverse (go (l-1)) rb
            return $ concat rb'

prune :: [Token String] -> SyntaxTree -> SyntaxTree
prune target (Node rh rbs) = Node rh (catMaybes $ go target <$> rbs)
    where
        align :: [Token String] -> [Token SyntaxTree] -> Maybe [([Token String], [Token SyntaxTree])]
        align word rb = 
            let is_non_term (NonTerm _) = True
                is_non_term _ = False
                
                pre = takeWhile is_non_term rb
                post = dropWhile is_non_term rb

            in  case post of
                    [] -> Just [(word, pre)]
                    (t:ts) -> 
                        let Term term = t
                            word_pre = takeWhile (/= Term term) word
                            word_post = dropWhile (/= Term term) word
                        in  case word_post of
                                [] -> Nothing
                                (w:ws) -> ((word_pre ++ [w], pre ++ [t]) :) <$> align ws ts

        go' :: ([Token String], [Token SyntaxTree]) -> [Token SyntaxTree]
        go' (word, tokens) = fmap (prune word) <$> tokens

        go :: [Token String] -> [Token SyntaxTree] -> Maybe [Token SyntaxTree]
        go word tokens = concatMap go' <$> align word tokens 

main :: IO ()
main = do
    (rules, target_word) <- parseHardError inputParser <$> readFile "input/19.txt"

    let rule_heads = distinct $ fst <$> rules
        target_word_tokenized = parseHardError (tokenizer rule_heads) target_word
        rules_tokenized = second (parseHardError (tokenizer rule_heads)) <$> rules

        grammar :: Grammar
        grammar = Map.unionsWith (++) . fmap singleton $ rules_tokenized
            where
                singleton (rh, rb) = Map.singleton rh [rb]

    putStr "Part 1: "
    print $ singleReplacementCount grammar target_word_tokenized

    putStr "Part 2: "
    let enum = enumerate $ prune target_word_tokenized $ syntaxTree grammar "e"

    for_ (take 100000 $ enum) print
