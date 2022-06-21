module Utils 
    ( natural
    , tile
    , withCoordinates
    , lexeme
    ) where

import Text.Parsec.String (Parser)
import Text.Parsec (many1, digit, many, char, sepBy, newline, (<|>), sepEndBy)

natural :: Parser Int
natural = read <$> many1 digit

lexeme :: Parser a -> Parser a
lexeme p = p <* many (char ' ')

tile :: Parser [[Bool]]
tile =
    let tileCell = True <$ char '#' <|> False <$ char '.'
        tileRow = many1 tileCell
    in tileRow `sepEndBy` newline

withCoordinates :: [[a]] -> [([Int],a)]
withCoordinates = go 0 0 where
    go x y [] = []
    go x y ([]:aas) = 
        go 0 (y+1) aas
    go x y ((a:as):aas) =
        ([x,y],a) : go (x+1) y (as:aas)
