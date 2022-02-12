module Day24 where
import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec (alphaNum, char, newline, string, choice, sepBy, option, many1, digit, manyTill, anyChar, (<|>), optional, skipMany)
import Data.Maybe (fromMaybe)
import Data.List (uncons)
import Data.Char (digitToInt)
import Data.Either (fromRight)

data Register
    = W | X | Y | Z
    deriving Show

data Variable
    = Val Int
    | Ref Register
    deriving Show

data ALU
    = Inp Register
    | Add Register Variable
    | Mul Register Variable
    | Div Register Variable
    | Mod Register Variable
    | Eql Register Variable
    deriving Show

binaryAluP :: (Register -> Variable -> ALU) -> Parser ALU
binaryAluP alu = do
    reg <- registerP
    char ' '
    alu reg <$> variableP

registerP :: Parser Register
registerP = choice 
    [ W <$ char 'w'
    , X <$ char 'x'
    , Y <$ char 'y'
    , Z <$ char 'z'
    ]

integerP :: Parser Int
integerP = do 
    sign <- option "" (string "-")
    digits <- many1 digit
    return $ read (sign ++ digits)

variableP :: Parser Variable
variableP = choice 
    [ Ref <$> registerP
    , Val <$> integerP
    ]

aluP :: Parser ALU
aluP = choice
    [ string "inp " >> (Inp <$> registerP)
    , string "add " >> binaryAluP Add
    , string "div " >> binaryAluP Div
    , string "eql " >> binaryAluP Eql
    , char 'm' >> choice
        [ string "ul " >> binaryAluP Mul
        , string "od " >> binaryAluP Mod
        ]
    ]

commentP :: Parser String
commentP = char '#' >> manyTill anyChar newline

aluProgramP :: Parser [ALU]
aluProgramP = aluP `sepBy` (newline >> skipMany commentP)

day24 :: IO ()
day24 = do
    programEither <- parseFromFile aluProgramP "24-input.txt"
    putStr $ fromRight "error" $ do 
        program <- programEither

        let modelNumber = [1,3,5,7,9,2,4,6,8,9,9,9,9,9]

            decInput :: [Int] -> [Int]
            decInput [] = []
            decInput (x:xs)
                | x > 1     = x-1 : xs
                | otherwise = 9 : decInput xs

            isModelNumber :: [Int] -> Bool
            isModelNumber inputSeq =
                let (_, _, (_,_,_,z)) = last
                        . takeWhile (\(cmds, _, _) -> not (null cmds))
                        . iterate compute
                        $ (program, inputSeq, (0,0,0,0))
                in z == 0

            bruteForce = 
                let modelNumberUpperBound = replicate 14 9
                in takeWhile (not . snd) 
                    . map (\num -> (num, isModelNumber num))
                    . iterate (reverse . decInput . reverse)
                    $ modelNumberUpperBound

        --return $ unlines $ map show program
        return 
            $ unlines
            . map (show . \(_, _, state) -> state)
            . takeWhile (\(cmds, _, _) -> not (null cmds))
            . iterate compute
            $ (program, modelNumber, (0,0,0,0))

type AluState = (Int, Int, Int, Int)

store :: Register -> Int -> AluState -> AluState
store W w' (w,x,y,z) = (w',x,y,z)
store X x' (w,x,y,z) = (w,x',y,z)
store Y y' (w,x,y,z) = (w,x,y',z)
store Z z' (w,x,y,z) = (w,x,y,z')

load :: Variable -> AluState -> Int
load (Val v) _ = v
load (Ref W) (w,_,_,_) = w
load (Ref X) (_,x,_,_) = x
load (Ref Y) (_,_,y,_) = y
load (Ref Z) (_,_,_,z) = z

compute :: ([ALU], [Int], AluState) -> ([ALU], [Int], AluState)
compute ([], inputSeq, state) = ([], inputSeq, state)
compute (Inp register : cmds, inputSeq, state) =
    let (value, inputSeqRest) = fromMaybe (0,[]) (uncons inputSeq)
        state' = store register value state
    in (cmds, inputSeqRest, state')
compute (Add register var : cmds, inputSeq, state) = 
    let value' = load (Ref register) state + load var state
        state' = store register value' state
    in (cmds, inputSeq, state')
compute (Mul register var : cmds, inputSeq, state) = 
    let value' = load (Ref register) state * load var state
        state' = store register value' state
    in (cmds, inputSeq, state')
compute (Div register var : cmds, inputSeq, state) = 
    let value' = load (Ref register) state `div` load var state
        state' = store register value' state
    in (cmds, inputSeq, state')
compute (Mod register var : cmds, inputSeq, state) = 
    let value' = load (Ref register) state `mod` load var state
        state' = store register value' state
    in (cmds, inputSeq, state')
compute (Eql register var : cmds, inputSeq, state) = 
    let valueL = load (Ref register) state
        valueR = load var state
        state' = if valueL == valueR 
            then store register 1 state
            else store register 0 state
    in (cmds, inputSeq, state')