module Day18 (day18) where
import Text.Parsec.String (Parser, parseFromFile)
import Data.Char (digitToInt)
import Text.Parsec.Char (digit)
import Text.Parsec (char, (<|>), newline, sepBy)
import Data.Either (fromRight)
data Snailfish
    = Leaf Int 
    | Node Snailfish Snailfish
    deriving Eq

indent :: String -> String
indent = unlines . map ('.':) . lines

showSF1 :: Snailfish -> [Char]
showSF1 (Leaf n) = show n
showSF1 (Node left right) =
    "[" ++ showSF1 left ++ "," ++ showSF1 right ++ "]"

showSF2 :: Snailfish -> String
showSF2 (Leaf n) = show n ++ "\n"
showSF2 (Node left right) =
    indent (showSF2 left ++ showSF2 right)

instance Show Snailfish where
    show = showSF1

snailfish :: Parser Snailfish
snailfish = 
    let leaf = Leaf . digitToInt <$> digit
        node = do
            char '['
            left <- snailfish
            char ','
            right <- snailfish
            char ']'
            return (Node left right)
    in leaf <|> node

day18 :: IO ()
day18 = do
    input <- parseFromFile (snailfish `sepBy` newline) "18-input.txt"
    putStr "Part 1: "
    putStrLn ""
    let out = unlines
            . map show
            . concat
            . take 50
            . iterate (\ss -> 
                let s0 = last ss 
                    s1 = explode s0
                    s2 = split s1
                in [s1,s2]
            )
            . (:[])
            . foldl1 Node <$> input
        
    case out of
        Right out' -> putStr out'
        Left _ -> putStr "error"

    putStrLn "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"

    putStr "Part 2: "

converge :: Eq a => (a -> a) -> a -> a
converge f a
    | a == f a  = a
    | otherwise = converge f (f a)

add :: Snailfish -> Snailfish -> Snailfish
add s1 s2 = converge (split . explode) (Node s1 s2)

split :: Snailfish -> Snailfish
split (Leaf n) =
    if n < 10 then 
        Leaf n
    else 
        Node (Leaf $ n `div` 2) (Leaf $ (n+1) `div` 2)
split (Node left right) =
    if left /= split left then 
        Node (split left) right
    else 
        Node left (split right)

explode :: Snailfish -> Snailfish
explode snail = 
    let go :: Int -> Snailfish -> (Snailfish, Maybe Int, Maybe Int)
        go _ (Leaf n) = 
            (Leaf n, Nothing, Nothing)
        go d (Node (Leaf n) (Leaf m)) = 
            if d < 4 then
                (Node (Leaf n) (Leaf m), Nothing, Nothing)
            else 
                (Leaf 0, Just n, Just m)
        go d (Node left right) = 
            let (left', ln, lm) = go (d+1) left
                (right', rn, rm) = go (d+1) right
            in if left /= left' then
                case lm of
                    Nothing -> (Node left' right, ln, lm)
                    Just m  -> (Node left' (addLeft m right), ln, Nothing)
            else if right /= right' then
                case rn of
                    Nothing -> (Node left right', rn, rm)
                    Just n  -> (Node (addRight n left) right', Nothing, rm)
            else 
                (Node left right, Nothing, Nothing)

        addRight n (Leaf m) = Leaf (n+m) 
        addRight n (Node left right) = 
            Node left (addRight n right)

        addLeft n (Leaf m) = Leaf (n+m)
        addLeft n (Node left right) = 
            Node (addLeft n left) right

        (result, _, _) = go 0 snail
    in result

magnitude :: Snailfish -> Int
magnitude (Leaf n) = n
magnitude (Node left right)
    = magnitude left * 3 
    + magnitude right * 2