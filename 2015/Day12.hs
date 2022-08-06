module Main where
import Text.Megaparsec (Parsec, (<|>), between, some, many, anySingleBut, sepBy, choice, errorBundlePretty, parse, manyTill, anySingle)
import Data.Void (Void)
import Text.Megaparsec.Char (string, space, char)
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Arrow (second)

data Json
    = JsonBool Bool
    | JsonString String
    | JsonNumber Float
    | JsonArray [Json]
    | JsonObject [(String, Json)]
    deriving (Show, Eq)

type Parser = Parsec Void String

parser :: Parser Json
parser =
    let lex = L.lexeme space

        number :: Parser Float
        number = lex $ L.signed space ((fromIntegral <$> L.decimal) <|> L.float)

        true  = lex (True  <$ string "true")
        false = lex (False <$ string "false")
        brackets = between (lex $ char '[') (lex $ char ']')
        braces = between (lex $ char '{') (lex $ char '}')
        comma = lex (char ',')

        stringLiteral :: Parser String
        stringLiteral = lex $ between (char '"') (char '"') (many (anySingleBut '"'))

        keyValuePair :: Parser (String, Json)
        keyValuePair = (,)
            <$> stringLiteral
            <*  lex (char ':')
            <*> jsonValue

        jsonBool = JsonBool <$> (true <|> false)
        jsonString = JsonString <$> stringLiteral
        jsonNumber  = JsonNumber <$> number
        jsonArray = JsonArray <$> brackets (jsonValue `sepBy` comma)
        jsonObject = JsonObject <$> braces (keyValuePair `sepBy` comma)

        jsonValue :: Parser Json
        jsonValue = choice [ jsonBool, jsonNumber, jsonString, jsonArray, jsonObject ]

    in  jsonObject <|> jsonArray

sumNumbers :: Json -> Float
sumNumbers (JsonNumber num) = num
sumNumbers (JsonArray entries) =
    sum (sumNumbers <$> entries)
sumNumbers (JsonObject kvPairs) =
    sum (sumNumbers . snd <$> kvPairs)
sumNumbers _ = 0

rejectRed :: Json -> Json
rejectRed (JsonObject kvPairs)
    | JsonString "red" `elem` (snd <$> kvPairs) = JsonObject []
    | otherwise = JsonObject (second rejectRed <$> kvPairs)
rejectRed (JsonArray entries) =
    JsonArray (rejectRed <$> entries)
rejectRed value = value

main :: IO ()
main = do
    input <- parse parser "" <$> readFile "2015/input/12.json"
    case input of
        Left err -> putStr (errorBundlePretty err)
        Right input -> do
            putStr "Part 1: "
            print (sumNumbers input)

            putStr "Part 2: "
            print (sumNumbers . rejectRed $ input)