module Main where

import qualified Data.Map as Map
import Text.Megaparsec (Parsec, sepBy, choice, some, (<|>), parse, errorBundlePretty, MonadParsec (try), skipMany)
import Data.Void (Void)
import Text.Megaparsec.Char (newline, space, string, lowerChar, char)
import Text.Megaparsec.Char.Lexer (decimal, lexeme)
import Data.Bits (Bits(complement, (.&.), (.|.), shiftL, shiftR))
import Data.Word (Word16)
import Data.Maybe (fromJust)
import Data.Function ((&))

type Parser a = Parsec Void String a

data Ref = Id String | Val Word16
    deriving Show

data Instr
    = UnaryInstr (Word16 -> Word16) Ref 
    | BinaryInstr (Word16 -> Word16 -> Word16) Ref Ref

instance Show Instr where
    show (UnaryInstr _ ref) = "Unary " ++ show ref
    show (BinaryInstr _ refL refR) = "Binary " ++ show refL ++ " + " ++ show refR

shiftL' :: Word16 -> Word16 -> Word16
shiftL' a b = shiftL a (fromIntegral b)

shiftR' :: Word16 -> Word16 -> Word16
shiftR' a b = shiftR a (fromIntegral b)

parser :: Parser [(String, Instr)]
parser =
    let lex = lexeme $ skipMany (char ' ')
        identifier = lex (some lowerChar)

        ref :: Parser Ref
        ref = (Val <$> lex decimal) <|> (Id <$> identifier)

        set = UnaryInstr id <$> ref
        not = UnaryInstr complement <$ lex (string "NOT") <*> ref
        and = BinaryInstr (.&.) <$> ref <* lex (string "AND") <*> ref
        or  = BinaryInstr (.|.) <$> ref <* lex (string "OR")  <*> ref
        lshift = BinaryInstr shiftL' <$> ref <* lex (string "LSHIFT") <*> ref
        rshift = BinaryInstr shiftR' <$> ref <* lex (string "RSHIFT") <*> ref

        withOutput :: Parser Instr -> Parser (String, Instr)
        withOutput p = 
            flip (,) <$> p <* lex (string "->") <*> identifier

        line :: Parser (String, Instr)
        line = choice (try . withOutput <$> [and, set, not, or, lshift, rshift])

    in  line `sepBy` newline

type Circuit = Map.Map String Instr

outputAt :: Ref -> Circuit -> Maybe Word16
outputAt (Val n) circuit = Just n
outputAt (Id i) circuit = 
    case Map.lookup i circuit of
        Nothing -> Nothing
        Just (UnaryInstr f ref) -> 
            f <$> outputAt ref circuit
        Just (BinaryInstr f refL refR) -> 
            f <$> outputAt refL circuit 
              <*> outputAt refR circuit

reduceAt :: Ref -> Circuit  -> Circuit
reduceAt (Val n) circuit = circuit
reduceAt (Id i) circuit =
    case Map.lookup i circuit of
        Nothing -> circuit
        Just (UnaryInstr f ref) ->
            let circuit' = reduceAt ref circuit 
                output = fromJust $ f <$> outputAt ref circuit'
                instr' = UnaryInstr id (Val output)
            in  Map.insert i instr' circuit'
        Just (BinaryInstr f refL refR) -> 
            let circuit' = circuit 
                    & reduceAt refL 
                    & reduceAt refR

                outputL = fromJust $ outputAt refL circuit'
                outputR = fromJust $ outputAt refR circuit'

                instr' = UnaryInstr id (Val (f outputL outputR))
            in  Map.insert i instr' circuit'

main :: IO ()
main = do
    input <- parse parser "" <$> readFile "input/07.txt"
    case input of 
        Left error -> putStr (errorBundlePretty error)
        Right input -> do
            let circuit = Map.fromList input 

            putStr "Part 1: "
            let output1 = circuit
                    & reduceAt (Id "a")
                    & outputAt (Id "a")
                    & fromJust
            print output1

            putStr "Part 2: "
            let circuit' = Map.insert "b" (UnaryInstr id (Val output1)) circuit
                output2 = circuit'
                    & reduceAt (Id "a")
                    & outputAt (Id "a")
                    & fromJust
            print output2