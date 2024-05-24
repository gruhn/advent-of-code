{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import Utils (Parser, parseFile)
import Data.List (intercalate, concat)
import Text.Megaparsec (sepBy, sepEndBy, some, oneOf)
import Text.Megaparsec.Char (newline, char)
import Text.Megaparsec.Char.Lexer (decimal)
import Prelude hiding (concat)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (guard)
import Control.Monad.Trans.State (StateT)
import qualified Control.Monad.Trans.State as StateT
import Data.Functor.Identity (Identity)
import Data.Maybe (catMaybes)
import Data.Foldable (traverse_, for_)
import RegExp (RegExp (..), intersection, toNFA, states)

data SymbolL = DotL | HashL | Wildcard
  deriving (Eq, Ord)

data SymbolR = DotR | HashR | ManyDot
  deriving (Eq, Ord)

instance Show SymbolL where
  show = \case
    DotL     -> "."
    HashL    -> "#"
    Wildcard -> "?"

instance Show SymbolR where
  show = \case
    DotR    -> "."
    HashR   -> "#"
    ManyDot -> "*"

type PatternL = [SymbolL]

type PatternR = [SymbolR]

parser :: Parser [(String, [Int])]
parser = line `sepEndBy` newline
  where
    line :: Parser (String, [Int])
    line = do
      left <- some (oneOf (".#?" :: String)) 
      char ' ' 
      right <- decimal `sepBy` char ','
      return (left, right)

toPatternL :: String -> PatternL
toPatternL = map from_char
  where
    from_char :: Char -> SymbolL
    from_char = \case
      '?' -> Wildcard
      '.' -> DotL
      '#' -> HashL
      ___ -> undefined

toPatternR :: [Int] -> PatternR
toPatternR counts = [ManyDot] ++ intercalate some_dot hash_blocks ++ [ManyDot]
  where
    some_dot = [ManyDot, DotR]
    hash_blocks = map (`replicate` HashR) counts

type State = (PatternL, PatternR)

match :: SymbolL -> SymbolR -> Maybe SymbolL
match left right = 
  case (left, right) of
    (Wildcard, DotR)    -> Just DotL
    (Wildcard, HashR)   -> Just HashL
    (Wildcard, ManyDot) -> Just DotL

    (DotL, DotR)    -> Just DotL
    (DotL, ManyDot) -> Just DotL
    (DotL, HashR)   -> Nothing

    (HashL, DotR)    -> Nothing
    (HashL, ManyDot) -> Nothing
    (HashL, HashR)   -> Just HashL

data Transition = Trans { getSymbol :: SymbolL, getState :: State }
  deriving Show

transitions :: State -> [Transition]
transitions state = catMaybes [seek, skip, stay]
  where
    seek = do
      (left:rest_left, right:rest_right) <- return state
      guard $ right /= ManyDot
      pair <- match left right
      return $ Trans pair (rest_left, rest_right)

    skip = do
      (left:rest_left, ManyDot:right:rest_right) <- return state
      pair <- match left right
      return $ Trans pair (rest_left, rest_right)

    stay = do
      (left:rest_left, ManyDot:rest_right) <- return state
      pair <- match left ManyDot
      return $ Trans pair (rest_left, ManyDot:rest_right)

alignments :: State -> Int
alignments state0 = StateT.evalState (align state0) Map.empty
  where
    align :: State -> StateT (Map State Int) Identity Int
    align ([], [])        = return 1
    align ([], [ManyDot]) = return 1
    align state = do
      table <- StateT.get
      case Map.lookup state table of
        Just path_count -> return path_count
        Nothing -> do 
          path_counts <- traverse (align . getState) (transitions state)
          StateT.modify (Map.insert state $ sum path_counts)
          return $ sum path_counts

-- data SymbolL = DotL | HashL | Wildcard
--   deriving (Eq, Ord)

-- data SymbolR = DotR | HashR | ManyDot
--   deriving (Eq, Ord)

fromPatternL :: PatternL -> RegExp
fromPatternL pattern =
  foldr1 Concat $ do 
    symbol <- pattern
    return $ case symbol of
      DotL  -> Char '.'
      HashL -> Char '#'
      Wildcard -> Union (Char '.') (Char '#')

fromPatternR :: PatternR -> RegExp
fromPatternR pattern =
  foldr1 Concat $ do 
    symbol <- pattern
    return $ case symbol of
      DotR  -> Char '.'
      HashR -> Char '#'
      ManyDot -> Star (Char '.')

main :: IO ()
main = do
  input <- parseFile parser "input/12.txt"

  -- putStr "Part 1: "
  -- print $ sum $ map alignments $ do 
  --   (str, hash_counts) <- input
  --   return (toPatternL str, toPatternR hash_counts)

  -- putStr "Part 2: "
  -- print $ sum $ map alignments $ do 
  --   (str, hash_counts) <- input
  --   let str_extended = intercalate "?" $ replicate 5 str
  --       hash_counts_extended = concat $ replicate 5 hash_counts
  --   return (toPatternL str_extended, toPatternR hash_counts_extended)

  for_ input $ \(str, hash_counts) -> do 
    let str_extended = intercalate "?" $ replicate 5 str
        hash_counts_extended = concat $ replicate 5 hash_counts

        reL = toNFA $ fromPatternL $ toPatternL str_extended
        reR = toNFA $ fromPatternR $ toPatternR hash_counts_extended

    print $ length $ states $ intersection reL reR

  
