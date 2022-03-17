module Day14 where

import qualified Data.Map as Map
import Data.Map (Map)
import Text.Parsec (many1, digit, string, count, oneOf, newline, sepBy, (<|>), try)
import Text.Parsec.String (Parser, parseFromFile)
import Data.Foldable (foldl')
import Data.Bits (setBit, clearBit)

type Memory = Map Int Int

data Instruction = Mask String | Mem Int Int

natural :: Parser Int
natural = read <$> many1 digit

parser :: Parser [Instruction]
parser =
    let mask :: Parser Instruction
        mask = Mask <$> (string "mask = " *> count 36 (oneOf "01X"))

        mem :: Parser Instruction
        mem = do
            addr <- string "mem[" *> natural <* string "]"
            string " = "
            Mem addr <$> natural

        instr :: Parser Instruction
        instr = try mem <|> mask

    in instr `sepBy` newline

type State = (Memory, String)

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

bitModifier1 :: Int -> Char -> (Int -> Int)
bitModifier1 i '1' = (`setBit` i)
bitModifier1 i '0' = (`clearBit` i)
bitModifier1 i 'X' = id
bitModifier1 i  _  = error "parse error"

applyBitMask1 :: String -> Int -> Int
applyBitMask1 = 
    compose . zipWith bitModifier1 (reverse [0..35])

eval1 :: State -> Instruction -> State 
eval1 (memory, mask) (Mem addr val) = 
    (Map.insert addr (applyBitMask1 mask val) memory, mask)
eval1 (memory, _) (Mask mask) = 
    (memory, mask)

bitModifier2 :: Int -> Char -> (Int -> [Int])
bitModifier2 i '1' addr = [addr `setBit` i]
bitModifier2 i '0' addr = [addr]
bitModifier2 i 'X' addr = [addr `setBit` i, addr `clearBit` i] 
bitModifier2 i  _  addr = error "parse error"

applyBitMask2 :: String -> Int -> [Int]
applyBitMask2 str addr =
    let bs = zipWith bitModifier2 (reverse [0..35]) str
    in foldr concatMap [addr] bs

eval2 :: State -> Instruction -> State
eval2 (memory, mask) (Mem addr val) =
    let addrs = applyBitMask2 mask addr
        memory' = foldr (`Map.insert` val) memory addrs 
    in (memory', mask)
eval2 (memory, _) (Mask mask) =
    (memory, mask)

day14 :: IO ()
day14 = do
    input <- parseFromFile parser "input/14.txt"

    putStr "Part 1: "
    let initState = (Map.empty, replicate 36 'X')
    print $ sum . fst . foldl' eval1 initState <$> input

    putStr "Part 2: "
    print $ sum . fst . foldl' eval2 initState <$> input