module Main where
import Text.Megaparsec (Parsec, choice, MonadParsec (try), sepBy, parse, errorBundlePretty, (<|>))
import Data.Void (Void)
import Text.Megaparsec.Char (string, newline, hspace, char)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import qualified Data.Map as Map
import Data.Foldable (for_)

type Parser = Parsec Void String

data Register = A | B
    deriving Show

data Instruction
    = Half Register
    | Triple Register
    | Inc Register
    | Jump Int
    | JumpIfEven Register Int
    | JumpIfOne Register Int
    deriving Show

type Program = Map.Map Int Instruction

parser :: Parser Program
parser =
    let register = (A <$ char 'a') <|> (B <$ char 'b')

        half = Half <$ string "hlf " <*> register
        triple = Triple <$ string "tpl " <*> register
        inc = Inc <$ string "inc " <*> register
        jump = Jump <$ string "jmp " <*> signed hspace decimal
        jumpIfEven = JumpIfEven <$ string "jie " <*> register <* string ", " <*> signed hspace decimal
        jumpIfOne = JumpIfOne <$ string "jio " <*> register <* string ", " <*> signed hspace decimal

        instr :: Parser Instruction
        instr = choice [ half, triple, inc, jump, jumpIfEven, jumpIfOne ]

        program :: Parser [Instruction]
        program = instr `sepBy` newline

    in  Map.fromList . zip [0..] <$> program

type State = (Int,(Int,Int))

load :: Register -> (Int, Int) -> Int
load A = fst
load B = snd

update :: Register -> (Int -> Int) -> (Int,Int) -> (Int,Int)
update A f (a, b) = (f a, b)
update B f (a, b) = (a, f b)

runInstr :: Instruction -> State -> State
runInstr (Half reg) (i, mem) =
    (i+1, update reg (`div` 2) mem)
runInstr (Triple reg) (i, mem) =
    (i+1, update reg (*3) mem)
runInstr (Inc reg) (i, mem) =
    (i+1, update reg (+1) mem)
runInstr (Jump j) (i, mem) =
    (i+j, mem)
runInstr (JumpIfEven reg j) (i, mem)
    | even (load reg mem) = (i+j, mem)
    | otherwise = (i+1, mem)
runInstr (JumpIfOne reg j) (i, mem)
    | 1 == load reg mem = (i+j, mem)
    | otherwise = (i+1, mem)

step :: Program -> State -> State
step program (i, mem) =
    case Map.lookup i program of
        Nothing    -> (i, mem)
        Just instr -> runInstr instr (i, mem)

converge :: Eq a => (a -> a) -> a -> a
converge f a
    | a == f a  = a
    | otherwise = converge f (f a)

run :: (Int, Int) -> Program -> (Int, Int)
run input program =
    let (_,output) = converge (step program) (0, input)
    in  output

main :: IO ()
main = do
    input <- parse parser "" <$> readFile "2015/input/23.txt"
    case input of
        Left err -> putStr (errorBundlePretty err)
        Right program -> do
            putStr "Part 1: "
            print $ run (0,0) program

            putStr "Part 2: "
            print $ run (1,0) program