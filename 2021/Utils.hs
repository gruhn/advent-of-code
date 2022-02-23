module Utils where
import Text.Parsec.String (Parser)
import Text.Parsec (option, many1, digit)
import Text.Parsec.Char (string)

converge :: Eq a => (a -> a) -> a -> a
converge f a
    | a == f a  = a
    | otherwise = converge f (f a)

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (a:as) =
    [ (a,a') | a' <- as ] ++ pairs as

integerP :: Parser Int
integerP = do 
    sign <- option "" (string "-")
    digits <- many1 digit
    return $ read (sign ++ digits)