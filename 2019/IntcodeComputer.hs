{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module IntcodeComputer (module IntcodeComputer) where

import Text.Megaparsec (Parsec, sepBy, parse, single, anySingle, count, (<|>), many, parseTest, errorBundlePretty)
import Data.Void (Void)
import Text.Megaparsec.Char.Lexer (signed, decimal)
import Text.Megaparsec.Char (char, space)
import Data.Functor (($>))
import qualified Data.Map as Map

type Program = Map.Map Integer Integer

programFrom :: [Integer] -> Program
programFrom = Map.fromList . zip [0..]

parser :: Parsec Void String Program
parser =
    let signedDecimal = signed space decimal
    in  programFrom <$> signedDecimal `sepBy` char ','

data Op
    = Add Integer Integer Integer
    | Mul Integer Integer Integer
    | Inp Integer
    | Out Integer
    | JIT Integer Integer
    | JIF Integer Integer
    | Lt Integer Integer Integer
    | Eq Integer Integer Integer
    | RelBase Integer
    | Halt
    deriving Show

-- >>> take 10 $ digits 1234
-- [4,3,2,1,0,0,0,0,0,0]

digits :: Integer -> [Integer]
digits x = x `mod` 10 : digits (x `div` 10)

getAt :: Program -> Integer -> Integer
getAt p i = Map.findWithDefault 0 i p

nextOp :: State -> Op
nextOp (State _ i p _ relBase) =
    let opcode = getAt p i
        arg1 = getAt p (i+1) 
        arg2 = getAt p (i+2) 
        arg3 = getAt p (i+3) 

        deref 0 ref = getAt p ref
        deref 1 val = val
        deref 2 ref = getAt p (ref + relBase)
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
            9 -> RelBase arg1
            99 -> Halt
            o -> error ("unknown op code: " ++ show o ++ " (pointer: " ++ show i ++ ")")
    in  op

data State = State 
    { input :: [Integer]
    , pointer :: Integer 
    , program :: Map.Map Integer Integer
    , output :: [Integer] 
    , relativeBase :: Integer
    } deriving (Show, Eq)

initialState :: Program -> [Integer] -> State
initialState program input = 
    State input 0 program [] 0

mapInput f s = s { input = f $ input s }
mapPointer f s = s { pointer = f $ pointer s }
mapProgram f s = s { program = f $ program s }
mapOutput f s = s { output = f $ output s }
mapRelativeBase f s = s { relativeBase = f $ relativeBase s }

boolToInt :: Bool -> Integer
boolToInt True = 1
boolToInt False = 0

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
    . mapProgram (Map.insert a3 (boolToInt $ a1 < a2))
applyOp (Eq a1 a2 a3) =
    mapPointer (+4) 
    . mapProgram (Map.insert a3 (boolToInt $ a1 == a2))
applyOp (RelBase a1) =
    mapPointer (+2)
    . mapRelativeBase (+a1)
applyOp Halt = id

step :: State -> State
step state = applyOp (nextOp state) state

converge :: Eq a => (a -> a) -> a -> a
converge f a
    | a == f a  = a
    | otherwise = converge f (f a)

run :: Program -> [Integer] -> [Integer]
run program input =
    let state0 = initialState program input 
        stateN = converge step state0
    in  output stateN