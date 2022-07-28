module GridParser (parser) where
import Text.Megaparsec (Parsec, (<|>), many, sepBy)
import Data.Void (Void)
import Text.Megaparsec.Char (char, newline)

type Point = (Int,Int)

grid :: [[Point]]
grid = (<$> [0..]) . flip (,) <$> [0..]

zip2D :: [[a]] -> [[b]] -> [[(a,b)]]
zip2D = zipWith zip

filter2D :: (a -> Bool) -> [[a]] -> [[a]]
filter2D p = fmap (filter p)

parser :: Parsec Void String [Point]
parser =
    let cell = (True <$ char '#') <|> (False <$ char '.')
        row = many cell

        points :: [[Bool]] -> [Point]
        points = fmap fst . concat . filter2D snd . zip2D grid

    in  points <$> row `sepBy` newline