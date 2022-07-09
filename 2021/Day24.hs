module Main where
import qualified Data.Map as Map
import Text.Megaparsec (Parsec, choice, (<|>), sepBy, try, parse, errorBundlePretty, oneOf)
import Data.Void (Void)
import Text.Megaparsec.Char (char, space, string, newline)
import Text.Megaparsec.Char.Lexer ( signed, decimal )

data Value = Ref Char | Val Integer
    deriving Show

data OpType = Add | Mul | Mod | Div | Eql
    deriving Show

data Instr = Inp Char | Op OpType Char Value
    deriving Show

type Parser a = Parsec Void String a

parser :: Parser [Instr]
parser =
    let integer :: Parser Integer
        integer = signed space decimal

        variable = oneOf "wxyz"

        value :: Parser Value
        value = (Ref <$> variable) <|> (Val <$> integer)

        add = Add <$ string "add"
        mul = Mul <$ string "mul"
        mod = Mod <$ string "mod"
        div = Div <$ string "div"
        eql = Eql <$ string "eql"

        opType = choice [add, try mul, mod, div, eql]

        inp = Inp <$ string "inp " <*> variable
        op  = Op <$> opType <* space <*> variable <* space <*> value

        instr = inp <|> op

    in  instr `sepBy` newline

data Range = Range Integer Integer 
    deriving Show

allEqual :: Eq a => [a] -> Bool
allEqual []     = True
allEqual (a:as) = all (==a) as

eval :: OpType -> Range -> Range -> Range
eval Add (Range x1 x2) (Range y1 y2) = 
    Range (x1+y1) (x2+y2)
eval Mul (Range x1 x2) (Range y1 y2) = 
    let zs = [x1*y1, x1*y2, x2*y1, x2*y2]
    in  Range (minimum zs) (maximum zs)
eval Mod (Range x1 x2) (Range y1 y2) = 
    Range 0 (x2)
eval Div (Range x1 x2) (Range y1 y2) = 
    let zs = [x1 `div` y1, x1 `div` y2, x2 `div` y1, x2 `div` y2]
    in  Range (minimum zs) (maximum zs)
eval Eql (Range x1 x2) (Range y1 y2)
    | x1 > y2 || x2 < y1     = Range 0 0
    | allEqual [x1,x2,y1,y2] = Range 1 1
    | otherwise              = Range 0 1

type State = Map.Map Char Range

valueOf :: State -> Value -> Range
valueOf state (Ref var) = 
    Map.findWithDefault (Range 0 0) var state
valueOf state (Val val) = Range val val

step :: State -> Instr -> State
step state (Inp reg) = 
    Map.insert reg (Range 1 9) state
step state (Op opType var ref) = 
    let range1 = Map.findWithDefault (Range 0 0) var state
        range2 = valueOf state ref
        range3 = eval opType range1 range2
    in  Map.insert var range3 state

run :: [Instr] -> State
run = foldl step Map.empty

main :: IO ()
main = do
    input <- parse parser "" <$> readFile "2021/24-input.txt"
    case input of
        Left error -> putStr (errorBundlePretty error)
        Right instructions -> print $ run instructions