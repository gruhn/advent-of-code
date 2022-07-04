module Main where
import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec (string, many1, digit, char, choice, newline, sepBy, (<|>), chainl1, many, parse)

data Expr =
    Val Int | Add Expr Expr | Mul Expr Expr | Parens Expr
    deriving Show

lexeme :: Parser a -> Parser a
lexeme p = p <* many (char ' ')

expr1 :: Parser Expr
expr1 = 
    let add = Add <$ lexeme (char '+')
        mul = Mul <$ lexeme (char '*')
        op = add <|> mul

        val = Val . read <$> lexeme (many1 digit)

        parens = Parens <$> 
            (lexeme (char '(') *> expr1 <* lexeme (char ')'))

        term = val <|> parens

    in chainl1 term op

expr2 :: Parser Expr
expr2 = 
    let add = Add <$ lexeme (char '+')
        mul = Mul <$ lexeme (char '*')

        val = Val . read <$> lexeme (many1 digit)

        parens = Parens <$> 
            (lexeme (char '(') *> expr2 <* lexeme (char ')'))

        term = val <|> parens

        sum = chainl1 term add

    in chainl1 sum mul

eval :: Expr -> Int
eval (Val x) = x
eval (Add l r) = eval l + eval r
eval (Mul l r) = eval l * eval r
eval (Parens x) = eval x

main :: IO ()
main = do
    input <- readFile "2020/input/18.txt"

    putStr "Part 1: "
    print $ sum . map eval <$> 
        parse (expr1 `sepBy` newline) "" input

    putStr "Part 2: "
    print $ sum . map eval <$> 
        parse (expr2 `sepBy` newline) "" input