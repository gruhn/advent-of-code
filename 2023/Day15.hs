module Main where
import Utils (Parser, parseFile)
import Text.Megaparsec (try, sepBy, some, (<|>))
import Text.Megaparsec.Char (lowerChar, newline, char)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Char (ord)
import Data.Foldable (foldl')
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Map (Map)

data Operation = Set String Int | Delete String

instance Show Operation where
  show (Set label val) = label ++ "=" ++ show val
  show (Delete label)  = label ++ "-"

parser :: Parser [Operation]
parser = operation `sepBy` char ',' <* newline
  where
    label = some lowerChar

    delete :: Parser Operation
    delete = Delete <$> label <* char '-'

    set :: Parser Operation
    set = Set <$> label <* char '=' <*> decimal

    operation :: Parser Operation
    operation = try delete <|> set

hash :: String -> Int
hash = foldl' (\val c -> (val + ord c) * 17 `rem` 256) 0

insertList :: (String,Int) -> [(String,Int)] -> [(String,Int)]
insertList (label,num) [] = [(label,num)]
insertList (label,num) ((label',num'):rest_list)
  | label == label' = (label,num) : rest_list
  | otherwise = (label',num') : insertList (label,num) rest_list

insert :: Map Int [(String,Int)] -> Operation -> Map Int [(String, Int)]
insert boxes (Set label num) = Map.alter go (hash label) boxes
  where
    go :: Maybe [(String, Int)] -> Maybe [(String,Int)]
    go = \case
      Nothing   -> Just [(label,num)]
      Just list -> Just $ insertList (label,num) list
insert boxes (Delete label) =
  Map.adjust (List.filter ((/= label) . fst)) (hash label) boxes

power :: Map Int [(String, Int)] -> Int
power = sum . Map.mapWithKey box_power
  where
    box_power :: Int -> [(String,Int)] -> Int
    box_power box_id = sum . zipWith (lens_power box_id) [0..]

    lens_power :: Int -> Int -> (String,Int)-> Int
    lens_power box_id index (_, focal_length) = (box_id + 1) * (index + 1) * focal_length

main :: IO ()
main = do
  input <- parseFile parser "input/15.txt"

  putStr "Part 1: "
  print $ sum $ map (hash . show) input

  putStr "Part 2: "
  print $ power $ foldl' insert Map.empty input
