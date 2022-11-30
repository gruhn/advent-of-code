module Main where
import Data.Tuple (swap)
import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec (digit, many1, newline, char, sepBy, (<|>))
import Data.Foldable (minimumBy)
import Data.Function

crt :: (Integral a, Foldable t) => t (a, a) -> a
crt xs = let (a,b) = foldr go (0, 1) xs in b - a
    where
    go (r1, m1) (r2, m2) = (r `mod` m, m)
        where
        r = r2 + m2 * (r1 - r2) * (m2 `inv` m1)
        m = m2 * m1

    -- Modular Inverse
    a `inv` m = let (_, i, _) = gcd a m in i `mod` m

    -- Extended Euclidean Algorithm
    gcd 0 b = (b, 0, 1)
    gcd a b = (g, t - (b `div` a) * s, s)
        where (g, s, t) = gcd (b `mod` a) a

data Bus = Bus Integer | OutOfService
    deriving Show

parser :: Parser (Integer, [Bus])
parser = do
    time <- read <$> many1 digit
    newline

    let oos = OutOfService <$ char 'x'
        bus = Bus . read <$> many1 digit
    
    busses <- (bus <|> oos) `sepBy` char ','

    return (time, busses)

activeWithIndex :: [Bus] -> [(Integer, Integer)]
activeWithIndex = foldr acc [] . zip [0..] where
    acc (i, Bus b) xs = (i, b) : xs
    acc (_, OutOfService) xs = xs

part1 :: (Integer, [Bus]) -> Integer
part1 (time, busses) =
    let busses' = map snd . activeWithIndex $ busses
        (busId, waitTime) = minimumBy (compare `on` snd) 
            $ map (\b -> (b, b - time `mod` b)) busses'
    in busId * waitTime

main :: IO ()
main = do
    input <- parseFromFile parser "input/13.txt"

    putStr "Part 1 : "
    print $ part1 <$> input

    putStr "Part 2 : "
    print $ crt . activeWithIndex . snd <$> input