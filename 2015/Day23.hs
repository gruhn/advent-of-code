module Main where
import Text.Megaparsec (Parsec, choice, MonadParsec (try), sepBy, parse, errorBundlePretty)
import Data.Void (Void)
import Text.Megaparsec.Char (string, lowerChar, newline, hspace)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import qualified Data.Map as Map
import Data.Foldable (for_)

type Parser = Parsec Void String

data Instruction
    = Half Char
    | Triple Char
    | Inc Char
    | Jump Int
    | JumpIfEven Char Int
    | JumpIfOdd Char Int
    deriving Show

type Program = Map.Map Int Instruction

parser :: Parser Program 
parser =
    let half = Half <$ string "hlf " <*> lowerChar
        triple = Triple <$ string "tpl " <*> lowerChar
        inc = Inc <$ string "inc " <*> lowerChar
        jump = Jump <$ string "jmp " <*> signed hspace decimal
        jumpIfEven = JumpIfEven <$ string "jie " <*> lowerChar <* string ", " <*> signed hspace decimal
        jumpIfOdd = JumpIfOdd <$ string "jio " <*> lowerChar <* string ", " <*> signed hspace decimal

        instr :: Parser Instruction
        instr = choice [ half, triple, inc, jump, jumpIfEven, jumpIfOdd ]

        program :: Parser [Instruction]
        program = instr `sepBy` newline

    in  Map.fromList . zip [0..] <$> program

type Memory = Map.Map Char Integer

type State = (Int, Memory)

runInstr :: Instruction -> State -> State
runInstr (Half reg) (i, mem) = 
    (i+1, Map.adjust (`div` 2) reg mem)
runInstr (Triple reg) (i, mem) = 
    (i+1, Map.adjust (*3) reg mem)
runInstr (Inc reg) (i, mem) = 
    (i+1, Map.adjust (+1) reg mem)
runInstr (Jump j) (i, mem) = 
    (i+j, mem)
runInstr (JumpIfEven reg j) (i, mem)
    | even (mem Map.! reg) = (i+j, mem)
    | otherwise = (i+1, mem)
runInstr (JumpIfOdd reg j) (i, mem)
    | odd (mem Map.! reg) = (i+j, mem)
    | otherwise = (i+1, mem)

step :: Program -> State -> State
step program (i, mem) =
    case Map.lookup i program of
        Nothing    -> (i,mem)
        Just instr -> runInstr instr (i, mem)

converge :: Eq a => (a -> a) -> a -> a
converge f a
    | a == f a  = a
    | otherwise = converge f (f a) 

run :: Program -> State
run program =
    let state0 = (0, Map.fromList [('a', 0), ('b', 0)])
    in  converge (step program) state0

main :: IO ()
main = do
    input <- parse parser "" <$> readFile "2015/input/23.txt"
    case input of
        Left err -> putStr (errorBundlePretty err)
        Right program -> do
            putStr "Part 1: "
            print $ run program
            -- for_ (Map.elems program) print