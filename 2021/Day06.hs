module Main where
import Text.Parsec.String (parseFromFile, Parser)
import Text.Parsec (sepBy, char, digit, many1)

natP :: Parser Integer
natP = read <$> many1 digit

main :: IO ()
main = do
    ages <- parseFromFile (natP `sepBy` char ',') "2021/06-input.txt"
    putStr "Part 1: "
    print $ sum . map (descendantCount 80) <$> ages
    putStr "Part 2: "
    print $ sum . map (descendantCount 256) <$> ages
  
binom :: Integer -> Integer -> Integer
binom n k 
    | k < 0     = 0
    | k > n     = 0
    | k > n-k   = product [(k+1)..n] `div` product [1..(n-k)]
    | otherwise = product [(n-k+1)..n] `div` product [1..k]
    
descendantCount :: Integer -> Integer -> Integer
descendantCount days age =
  let -- remaining days adjusted for age:
      days' = days - (age + 1)
      -- maximum number of 8 day periods in ancestor chain:
      max8 = days' `div` (8+1)
      -- maximum number of 6 day periods in ancestor chain given fixed number of 8s:
      max6 n = (days' - (8+1)*n) `div` (6+1)
  in 1 + sum [ (n+m) `binom` n | n <- [0 .. max8], m <- [0 .. max6 n] ]

