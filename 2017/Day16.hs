{-# LANGUAGE FlexibleInstances #-}
module Main where
import Utils (Parser, parseFile)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (char, lowerChar)
import Text.Megaparsec (sepBy, choice)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Either (partitionEithers)
import Data.Semigroup (stimesMonoid)

data Move = Spin Int | Exchange Int Int | Partner Char Char

parser :: Parser [Move]
parser = move `sepBy` char ','
 where
  move :: Parser Move
  move = choice 
   [ Spin     <$ char 's' <*> decimal
   , Exchange <$ char 'x' <*> decimal   <* char '/' <*> decimal
   , Partner  <$ char 'p' <*> lowerChar <* char '/' <*> lowerChar
   ]

newtype Swap a = Swap { getMap :: Map a a }
 deriving Show

instance Semigroup (Swap Int) where
  Swap mapA <> Swap mapB = Swap (mapA `Map.compose` mapB)

instance Semigroup (Swap Char) where
  Swap mapA <> Swap mapB = Swap (mapB `Map.compose` mapA)

instance Monoid (Swap Int) where
  mempty = Swap $ Map.fromList [ (i,i) | i <- [0 .. 15] ]

instance Monoid (Swap Char) where
 mempty = Swap $ Map.fromList [ (c,c) | c <- ['a' .. 'p'] ]

toSwap :: Move -> Either (Swap Char) (Swap Int)
toSwap = \case
 Partner a b  -> Left  $ Swap $ Map.fromList [(a,b),(b,a)] <> getMap mempty
 Exchange i j -> Right $ Swap $ Map.fromList [(i,j),(j,i)] <> getMap mempty
 Spin n       -> Right $ Swap $ Map.fromList [ ((i+n) `mod` 16, i) | i <- [0 .. 15] ]

main :: IO ()
main = do
 moves <- parseFile parser "input/16.txt"

 let start_state :: Map Int Char
     start_state = Map.fromList $ zip [0 ..] ['a' .. 'p']

     (name_swaps, index_swaps) = partitionEithers $ map toSwap moves

     index_swaps_chained = mconcat index_swaps
     name_swaps_chained = mconcat name_swaps

     index_swaps_exp = stimesMonoid (10^9) index_swaps_chained
     name_swaps_exp = stimesMonoid (10^9) name_swaps_chained

 putStr "Part 1: "
 putStrLn 
  $ Map.elems 
  $ getMap name_swaps_chained `Map.compose` start_state `Map.compose` getMap index_swaps_chained

 putStr "Part 2: "
 putStrLn
  $ Map.elems
  $ getMap name_swaps_exp `Map.compose` start_state `Map.compose` getMap index_swaps_exp
