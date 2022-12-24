module Utils where

import Text.Megaparsec (Parsec, parse, errorBundlePretty)
import Data.Void (Void)
import qualified Data.Set as S
import Data.Set (Set)

type Parser = Parsec Void String

parseHardError :: Parser a -> String -> a
parseHardError parser input = 
  case parse parser "" input of
    Left  err    -> error (errorBundlePretty err)
    Right output -> output

converge :: Eq a => (a -> a) -> a -> a
converge f a
    | a == f a  = a
    | otherwise = converge f (f a)

fixpointM :: (Eq a, Monad m) => (a -> m a) -> a -> m a
fixpointM f a = do
  a' <- f a
  if a' == a then
    return a
  else 
    fixpointM f a'

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p []     = []
takeUntil p (a:as) = 
  a : if p a then [] else takeUntil p as

zipWith2D :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipWith2D f = zipWith (zipWith f)

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n as = 
  take n as : chunksOf n (drop n as)

withCoordinates :: [[a]] -> [((Int,Int), a)]
withCoordinates rows = do
  (y, row)  <- zip [0..] rows
  (x, cell) <- zip [0..] row
  return ((x,y), cell)

takeDistinct :: Ord a => [a] -> [a]
takeDistinct = go S.empty 
  where
    go seen [] = []
    go seen (a:as)
      | S.member a seen = []
      | otherwise = a : go (S.insert a seen) as