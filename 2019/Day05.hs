{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Main where

import Text.Megaparsec (Parsec, sepBy, parse, single, anySingle, count, (<|>), many, parseTest, errorBundlePretty)
import Data.Void (Void)
import Text.Megaparsec.Char.Lexer (signed, decimal)
import Text.Megaparsec.Char (char, space)
import Data.Functor (($>))
import Control.Arrow (Arrow(first))
import qualified Data.Map as Map
import Data.Bifunctor (bimap)
import Data.Function (on)
import Data.List (sortBy)
import Data.Foldable (find)

type Program = Map.Map Int Int

parser :: Parsec Void String Program
parser =
    let signedDecimal = signed space decimal
        programFrom = Map.fromList . zip [0..]
    in programFrom <$> signedDecimal `sepBy` char ','

data Op
    = Add Int Int Int
    | Mul Int Int Int
    | Inp Int
    | Out Int
    | Halt
    deriving Show

digits :: Int -> [Int]
digits x = x `mod` 10 : digits (x `div` 10)

parseOp :: Int -> Program -> Op
parseOp i p =
    let opcode = p Map.! i
        c = p Map.! (i+1) 
        b = p Map.! (i+2) 
        a = p Map.! (i+3) 
        
        resolve 0 ref = p Map.! ref
        resolve 1 val = val
        resolve _ _ = error "unknown arg mode"

        ds = digits opcode

        opType = (ds !! 0) + (ds !! 1) * 10
        argA = resolve (ds !! 2) a
        argB = resolve (ds !! 3) b
        argC = resolve (ds !! 4) c

        op = case opType of
            1 -> Add argC argB argA
            2 -> Mul argC argB argA
            3 -> Inp argC
            4 -> Out argC
            99 -> Halt
            _ -> error "unkown op code"
    in  op

type State = ([Int], Int, Program, [Int])

applyOp :: Op -> State -> State
applyOp (Add c b a) (inp, i, p, out) = 
    (inp, i+4, Map.insert a (c+b) p, out)
applyOp (Mul c b a) (inp, i, p, out) = 
    (inp, i+4, Map.insert a (c*b) p, out)
applyOp (Inp c) (inp, i, p, out) = 
    (tail inp, i+2, Map.insert c (head inp) p, out)
applyOp (Out c) (inp, i, p, out) =  
    (inp, i+2, p, c:out)
applyOp Halt state = state

step :: ([Int], Int, Program, [Int]) -> ([Int], Int, Program, [Int])
step state@(inp, i, p, out) = applyOp (parseOp i p) state

converge :: Eq a => (a -> a) -> a -> a
converge f a
    | a == f a  = a
    | otherwise = converge f (f a)

-- solve :: Program -> IO ()
-- solve program = do
--     putStr "Part 1: "
--     print $ outputOf program (12,2)
--     putStr "Part 2: "
--     let nounVerbPairs = crossProduct [0..99] [0..99]
--         result = find ((==19690720) . outputOf program) nounVerbPairs
--         score (noun, verb) = 100*noun + verb
--     print $ score <$> result

main :: IO ()
main = do
    input <- parse parser "" <$> readFile "2019/input/05.txt"
    case input of
        Left error -> putStr (errorBundlePretty error)
        Right program -> print $ converge step ([1], 0, program, [])