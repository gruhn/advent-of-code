module Main where
import Utils (Parser, parseFile)
import Text.Megaparsec (between, (<|>), anySingle, sepBy, many, noneOf, some)
import Text.Megaparsec.Char (char)
import Data.Text (Text)
import qualified Data.Text as Text

data Tree = Leaf Text | Node [Tree]
 deriving Show

parser :: Parser Tree
parser = tree
 where
  tree :: Parser Tree
  tree = leaf <|> node

  angle = between (char '<') (char '>')
  curly = between (char '{') (char '}')

  node :: Parser Tree
  node = curly $ Node <$> tree `sepBy` char ','

  leaf :: Parser Tree
  leaf = angle $ Leaf . mconcat <$> many (canceled <|> text)

  text :: Parser Text
  text = fmap Text.pack $ some $ noneOf (">!" :: String)

  canceled :: Parser Text
  canceled = char '!' >> anySingle >> ""

score :: Tree -> Int
score = go 1
 where
  go :: Int -> Tree -> Int
  go _ (Leaf _) = 0
  go depth (Node trees) =
   depth + sum [ go (depth+1) t | t <- trees ]

collectText :: Tree -> Text
collectText (Leaf str) = str
collectText (Node trees) = foldMap collectText trees

main :: IO ()
main = do
 tree <- parseFile parser "input/09.txt"

 putStr "Part 1: "
 print $ score tree

 putStr "Part 2: "
 print $ Text.length $ collectText tree
