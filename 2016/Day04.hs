module Main where
import Utils (Parser, parseHardError)
import Control.Applicative (some)
import Text.Megaparsec (sepEndBy, between, sepBy)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (lowerChar, char, newline)
import Data.List (sort, group, sortOn, intersperse, intercalate, isInfixOf, find)
import Control.Arrow (Arrow ((&&&)))
import Data.Ord (Down(Down))

data Room = Room
  { name :: [String]
  , sectorID :: Int
  , savedChecksum :: String }
  deriving Show

parser :: Parser [Room]
parser = room `sepBy` newline
  where
    room_name = some lowerChar `sepEndBy` char '-'
    room_sector_id = decimal
    room_checksum = between (char '[') (char ']') (some lowerChar)

    room = Room
      <$> room_name
      <*> room_sector_id
      <*> room_checksum

checksum :: Room -> String
checksum (Room name _ _)
  = take 5 . fmap snd                     -- only keep the top 5 chars
  $ sortOn (Down . fst) . sortOn snd      -- sort primary by frequency and secondary alphabetically
  $ fmap (length &&& head) . group . sort -- compute pairs of chars with their frequency
  $ concat name                           -- distinction between dash separated sections is not necessary

isValidRoom :: Room -> Bool
isValidRoom room = checksum room == savedChecksum room

-- >>> decrypt 3 "abcdefg"
-- "defghij"

decryptRoom :: Room -> Room
decryptRoom room =
  room { name = decrypt (sectorID room) <$> name room } 

decrypt :: Int -> String -> String
decrypt shift str = decryptChar <$> str
  where
    decryptChar :: Char -> Char
    decryptChar c = c_shifted
      where
        chars_after_c = dropWhile (/= c) $ cycle ['a' .. 'z']
        c_shifted = chars_after_c !! shift

main :: IO ()
main = do
  rooms <- parseHardError parser <$> readFile "2016/input/04.txt"

  putStr "Part 1: "
  let valid_rooms = filter isValidRoom rooms
  print 
    $ sum . fmap sectorID 
    $ valid_rooms

  putStr "Part 2: "
  print 
    $ find (("northpole" `isInfixOf`) . unwords . name)
    $ fmap decryptRoom valid_rooms
