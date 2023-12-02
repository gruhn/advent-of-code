module Main where
import Utils (Parser, parseFile)
import Text.Megaparsec (sepEndBy, sepBy, choice)
import Text.Megaparsec.Char (newline, string)
import Text.Megaparsec.Char.Lexer (decimal)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad (guard)

data Color = Red | Green | Blue
  deriving (Show, Eq, Ord)

data Game = Game Int [Map Color Int]

parser :: Parser [Game]
parser = game `sepEndBy` newline
  where
    game_id :: Parser Int
    game_id = string "Game " *> decimal <* string ": "

    game :: Parser Game
    game = Game <$> game_id <*> game_round `sepBy` string "; "

    cube_color :: Parser Color
    cube_color = choice
      [ Green <$ string "green" 
      , Blue  <$ string "blue" 
      , Red   <$ string "red" ]

    game_round :: Parser (Map Color Int)
    game_round = Map.fromList <$> cube `sepBy` string ", "

    cube :: Parser (Color, Int)
    cube = do
      count <- decimal
      string " "
      color <- cube_color
      return (color, count)

maxCubeCounts :: [Game] -> [(Int,Int,Int,Int)]
maxCubeCounts games = do
  Game game_id rounds <- games
  let max_per_color = Map.unionsWith max rounds
      max_red   = Map.findWithDefault 0 Red max_per_color
      max_green = Map.findWithDefault 0 Green max_per_color
      max_blue  = Map.findWithDefault 0 Blue max_per_color
  return (game_id, max_red, max_green, max_blue)

main :: IO ()
main = do
  games <- parseFile parser "input/02.txt"

  putStr "Part 1: "
  print $ sum $ do
    (game_id, red, green, blue) <- maxCubeCounts games
    guard $ red   <= 12
    guard $ green <= 13
    guard $ blue  <= 14
    return game_id

  putStr "Part 2: "
  print $ sum $ do
    (game_id, red, green, blue) <- maxCubeCounts games
    return $ red * green * blue
