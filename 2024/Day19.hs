module Main (main) where

import Utils (Parser, parseFile, countBy)
import Text.Megaparsec.Char (string, lowerChar, newline)
import Control.Applicative (some)
import Text.Megaparsec (sepBy, sepEndBy)
import Data.Map (Map)
import Control.Monad.Trans.State (State)
import qualified Data.Map as Map
import qualified Control.Monad.Trans.State as State
import qualified Trie
import Trie (Trie)

parser :: Parser ([String], [String])
parser = do
  patterns <- some lowerChar `sepBy` string ", "
  string "\n\n"
  designs <- some lowerChar `sepEndBy` newline
  return (patterns, designs)

matchCount :: Trie Char -> String -> Int
matchCount patterns full_design = State.evalState (check_all full_design) Map.empty
  where
    check_all :: String -> State (Map String Int) Int
    check_all []     = return 1
    check_all design = do
      maybe_result <- State.gets (Map.lookup design)
      case maybe_result of
        Just result -> return result
        Nothing     -> do 
          result <- sum <$> traverse check_all (Trie.stripPrefixes patterns design)
          State.modify (Map.insert design result)
          return result

main :: IO ()
main = do
  (patterns, designs) <- parseFile parser "input/19.txt"

  let trie = Trie.fromList patterns
      match_counts = map (matchCount trie) designs

  putStr "Part 1: "
  print $ countBy (0<) match_counts

  putStr "Part 2: "
  print $ sum match_counts
