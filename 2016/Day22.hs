module Main where
import Utils (Parser, parseHardError)
import Text.Megaparsec (sepBy, some, anySingleBut, skipSome, manyTill, anySingle, (<|>), oneOf)
import Text.Megaparsec.Char (newline, hspace, char, letterChar, string)
import Text.Megaparsec.Char.Lexer (lexeme, decimal)
import Control.Monad (void, guard)
import Data.Foldable (maximumBy)
import Data.Function (on)

data Node = Node 
  { position :: (Int,Int)
  , sizeTerabyte :: Int 
  , usedTerabyte :: Int 
  , availTerabyte :: Int
  , usePercent :: Int
  } deriving (Eq, Show)

parser :: Parser [Node]
parser = line *> line *> node `sepBy` newline
  where
    line :: Parser String
    line = anySingle `manyTill` newline

    lex :: Parser a -> Parser a
    lex = lexeme (void hspace)  

    position :: Parser (Int,Int)
    position = lex $ do
      string "/dev/grid/node-x"
      x <- decimal
      string "-y"
      y <- decimal
      return (x,y)

    terabyte :: Parser Int
    terabyte = lex (decimal <* char 'T')

    percent :: Parser Int
    percent = lex (decimal <* char '%')

    node :: Parser Node
    node = Node 
      <$> position
      <*> terabyte
      <*> terabyte
      <*> terabyte
      <*> percent

viablePairs :: [Node] -> [(Node, Node)] 
viablePairs nodes = do
  a <- nodes
  guard (usedTerabyte a > 0)
  b <- nodes
  guard (a /= b)
  guard (usedTerabyte a <= availTerabyte b)
  return (a,b)

type State = [Node]

nextStates :: State -> [State]
nextStates = undefined

isFinalState :: State -> Bool
isFinalState = undefined

main :: IO ()
main = do
  nodes <- parseHardError parser <$> readFile "2016/input/22.txt"

  -- putStr "Part 1: "
  -- print $ length $ viablePairs nodes

  putStr "Part 2: "
  let target_pos = (0,0)
      source_pos = maximumBy (compare `on` fst) 
        $ filter ((==0) . snd) 
        $ position <$> nodes

  print source_pos
