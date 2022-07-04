{-# LANGUAGE TupleSections #-}
module Main where
import Text.Megaparsec (Parsec, sepBy, parse, single, anySingle, count, (<|>), many, parseTest, errorBundlePretty, VisualStream)
import Data.Void (Void)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (char)
import Data.Functor (($>))
import Control.Arrow (Arrow(first))
import qualified Data.Map as Map
import Data.Bifunctor (bimap)
import Data.Function (on)
import Data.List (sortBy)
import Data.Foldable (find)

type Program = Map.Map Int Int

programFrom :: [Int] -> Program
programFrom = Map.fromList . zip [0..]

parser :: Parsec Void String Program
parser = programFrom <$> decimal `sepBy` char ','

applyOp :: (Int -> Int -> Int) -> Int -> Program -> Program
applyOp op i p =
    let i1 = p Map.! (i+1)
        i2 = p Map.! (i+2)
        i3 = p Map.! (i+3)
        x1 = p Map.! i1
        x2 = p Map.! i2
        x3 = x1 `op` x2
    in Map.insert i3 x3 p

step :: (Int, Program) -> (Int, Program)
step (i, program) =
    case program Map.! i of
        99 -> (i, program)
        1 -> (i+4, applyOp (+) i program)
        2 -> (i+4, applyOp (*) i program)
        _ -> error "unknown op code"

converge :: Eq a => a -> (a -> a) -> a
converge a f
    | a == f a  = a
    | otherwise = converge (f a) f

outputOf :: Program -> (Int, Int) -> Int
outputOf p0 (noun, verb) =
    let p1 = Map.insert 1 noun p0 
        p2 = Map.insert 2 verb p1
        (_, p3) = converge (0, p2) step
    in  p3 Map.! 0

crossProduct :: [a] -> [b] -> [(a,b)]
crossProduct [] _ = []
crossProduct _ [] = []
crossProduct (a:as) bs =
    map (a,) bs ++ crossProduct as bs

solve :: Program -> IO ()
solve program = do
    putStr "Part 1: "
    print $ outputOf program (12,2)

    putStr "Part 2: "
    let nounVerbPairs = crossProduct [0..99] [0..99]
        result = find ((==19690720) . outputOf program) nounVerbPairs
        score (noun, verb) = 100*noun + verb

    print $ score <$> result

main :: IO ()
main = do 
    input <- parse parser "" <$> readFile "2019/input/02.txt"
    case input of
        Left error -> putStr (errorBundlePretty error)
        Right program -> solve program