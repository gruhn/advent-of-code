module Day16 where

import Text.Parsec
import Text.Parsec.String 
import Data.Char (digitToInt)
import Data.Either (fromRight)

hexToBin :: Parser String
hexToBin = choice $ map (\(input, output) -> char input >> return output)
    [ ('0', "0000"), ('1', "0001"), ('2', "0010"), ('3', "0011")
    , ('4', "0100"), ('5', "0101"), ('6', "0110"), ('7', "0111")
    , ('8', "1000"), ('9', "1001"), ('A', "1010"), ('B', "1011")
    , ('C', "1100"), ('D', "1101"), ('E', "1110"), ('F', "1111")
    ]

hexString :: Parser String
hexString = concat <$> many hexToBin

binToDec :: [Int] -> Int
binToDec [] = 0
binToDec (x:xs) = x * 2 ^ length xs + binToDec xs

type Packet = (Int, PacketBody)
data PacketBody = Value Int | Operator Int [Packet]
    deriving Show

binDigit :: Parser Int
binDigit = digitToInt <$> (char '0' <|> char '1')

packet :: Parser Packet
packet = do
    version <- binToDec <$> count 3 binDigit
    typeId <- binToDec <$> count 3 binDigit
    body <- packetBody typeId
    return (version, body)

packetBody :: Int -> Parser PacketBody
packetBody 4 = do
    initGroups <- many (char '1' >> count 4 binDigit)
    lastGroup <- char '0' >> count 4 binDigit
    let binValue = concat (initGroups ++ [lastGroup])
    return $ Value (binToDec binValue)
packetBody typeId = do 
    lengthTypeId <- binDigit
    operatorPacket typeId lengthTypeId

operatorPacket :: Int -> Int -> Parser PacketBody
operatorPacket typeId 0 = do
    totalLength <- binToDec <$> count 15 binDigit
    subString <- count totalLength anyChar
    case parse (many packet <* eof) "" subString of
        Right subPackets -> return $ Operator typeId subPackets
        Left error -> unexpected (show error)
operatorPacket typeId 1 = do
    packetCount <- binToDec <$> count 11 binDigit
    subPackets <- count packetCount packet
    return $ Operator typeId subPackets
operatorPacket _ _ = unexpected "length type ID not binary"

versions :: [Packet] -> [Int]
versions [] = []
versions ((version, Value _):rest) =
    version : versions rest
versions ((version, Operator _ subPackets):rest) =
    version : versions subPackets ++ versions rest

compute :: Packet -> Int
compute (_, Value val) = val
compute (_, Operator 0 packets) = 
    sum $ map compute packets
compute (_, Operator 1 packets) = 
    product $ map compute packets
compute (_, Operator 2 packets) = 
    minimum $ map compute packets
compute (_, Operator 3 packets) = 
    maximum $ map compute packets
compute (_, Operator 5 packets) =
    let [p1, p2] = map compute packets
    in if p1 > p2 then 1 else 0
compute (_, Operator 6 packets) =
    let [p1, p2] = map compute packets
    in if p1 < p2 then 1 else 0
compute (_, Operator 7 packets) =
    let [p1, p2] = map compute packets
    in if p1 == p2 then 1 else 0

main :: IO ()
main = do
    inputRaw <- readFile "16-input.txt"
    let parsed = do 
        hexParsed <- parse (hexString <* eof) "" inputRaw
        parse (many (try packet)) "" hexParsed

    putStr "Part 1: "
    print $ sum . versions <$> parsed
    putStr "Part 2: "
    print $ map compute <$> parsed