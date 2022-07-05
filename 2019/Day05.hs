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
    | JIT Int Int
    | JIF Int Int
    | Lt Int Int Int
    | Eq Int Int Int
    | Halt
    deriving Show

-- >>> take 10 $ digits 1234
-- [4,3,2,1,0,0,0,0,0,0]

digits :: Int -> [Int]
digits x = x `mod` 10 : digits (x `div` 10)

nextOp :: State -> Op
nextOp (State _ i p _) =
    let opcode = p Map.! i
        arg1 = p Map.! (i+1) 
        arg2 = p Map.! (i+2) 
        arg3 = p Map.! (i+3) 

        deref 0 ref = p Map.! ref
        deref 1 val = val
        deref _ _ = error "unknown arg mode"

        ds = digits opcode

        opType = (ds !! 0) + (ds !! 1) * 10
        mode1 = ds !! 2
        mode2 = ds !! 3
        mode3 = ds !! 4

        op = case opType of
            1 -> Add (deref mode1 arg1) (deref mode2 arg2) arg3
            2 -> Mul (deref mode1 arg1) (deref mode2 arg2) arg3
            3 -> Inp arg1
            4 -> Out (deref mode1 arg1)
            5 -> JIT (deref mode1 arg1) (deref mode2 arg2) 
            6 -> JIF (deref mode1 arg1) (deref mode2 arg2)
            7 -> Lt (deref mode1 arg1) (deref mode2 arg2) arg3
            8 -> Eq (deref mode1 arg1) (deref mode2 arg2) arg3
            99 -> Halt
            o -> error ("unknown op code: " ++ show o ++ " (pointer: " ++ show i ++ ")")
    in  op
data State = State 
    { input :: [Int]
    , pointer :: Int 
    , program :: Map.Map Int Int
    , output :: [Int] 
    } deriving (Show, Eq)

mapInput f s = s { input = f $ input s }
mapPointer f s = s { pointer = f $ pointer s }
mapProgram f s = s { program = f $ program s }
mapOutput f s = s { output = f $ output s }

applyOp :: Op -> State -> State
applyOp (Add a1 a2 a3) =
    mapProgram (Map.insert a3 (a1+a2))
    . mapPointer (+4)
applyOp (Mul a1 a2 a3) = 
    mapPointer (+4)
    . mapProgram (Map.insert a3 (a1*a2))
applyOp (Inp a1) = \state ->
    mapPointer (+2)
    . mapInput tail
    . mapProgram (Map.insert a1 (head $ input state)) 
    $ state
applyOp (Out a1) =
    mapPointer (+2) 
    . mapOutput (a1 :)
applyOp (JIT a1 a2)
    | 0 < a1    = mapPointer (const a2)
    | otherwise = mapPointer (+3)
applyOp (JIF a1 a2)
    | a1 == 0   = mapPointer (const a2)
    | otherwise = mapPointer (+3) 
applyOp (Lt a1 a2 a3) =
    mapPointer (+4) 
    . mapProgram (Map.insert a3 (fromEnum $ a1 < a2))
applyOp (Eq a1 a2 a3) =
    mapPointer (+4) 
    . mapProgram (Map.insert a3 (fromEnum $ a1 == a2))
applyOp Halt = id

step :: State -> State
step state = applyOp (nextOp state) state

converge :: Eq a => (a -> a) -> a -> a
converge f a
    | a == f a  = a
    | otherwise = converge f (f a)

main :: IO ()
main = do
    input <- parse parser "" <$> readFile "2019/input/05.txt"
    case input of
        Left error -> putStr (errorBundlePretty error)
        Right program -> do
            putStr "Part 2: "

            let state0 = State [5] 0 program []
                stateN = converge step state0

            print $ output stateN