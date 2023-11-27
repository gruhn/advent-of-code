module Utils where

import qualified Text.Megaparsec as P
import Data.Void (Void)
import qualified Data.Set as S
import Data.Function (on)
import Data.Foldable (maximumBy, toList)
import Data.List (group, sort)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char (hspace1)
import Data.Text (Text)
import qualified Data.Text.IO as TextIO

type Parser = P.Parsec Void Text

space :: Parser ()
space = L.space hspace1 P.empty P.empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space

integer :: Parser Int
integer = L.signed space (lexeme L.decimal)

parse :: Parser a -> Text -> a
parse parser input =
  case P.parse parser "" input of
    Left  err    -> error (P.errorBundlePretty err)
    Right output -> output

parseFile :: Parser a -> String -> IO a
parseFile parser path = parse parser <$> TextIO.readFile path

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
takeUntil _ []     = []
takeUntil p (a:as) =
  a : if p a then [] else takeUntil p as

takeWhileJust :: [Maybe a] -> [a]
takeWhileJust [] = []
takeWhileJust (Nothing : _) = []
takeWhileJust (Just a : mas) = a : takeWhileJust mas

zipWith2D :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipWith2D f = zipWith (zipWith f)

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n as = chunk : chunksOf n rest
  where (chunk, rest) = splitAt n as

takeDistinct :: Ord a => [a] -> [a]
takeDistinct = go S.empty
  where
    go _ [] = []
    go seen (a:as)
      | S.member a seen = []
      | otherwise = a : go (S.insert a seen) as

withCoords :: [[a]] -> [((Int,Int), a)]
withCoords rows = do
  (y, row)  <- zip [0..] rows
  (x, cell) <- zip [0..] row
  return ((x,y), cell)

(.*) :: (a -> b) -> (c -> d -> a) -> (c -> d -> b)
(.*) f g x y = f (g x y)

mostCommon :: Ord a => [a] -> Maybe a
mostCommon [] = Nothing
mostCommon xs = Just
  $ head
  $ maximumBy (compare `on` length)
  $ group
  $ sort xs

maximumBounded :: (Bounded a, Foldable t, Ord a) => t a -> a
maximumBounded as
  | null as   = minBound
  | otherwise = maximum as

safeMinimum :: (Ord a, Foldable t) => t a -> Maybe a
safeMinimum as
  | null as   = Nothing
  | otherwise = Just (minimum as)

combinations :: [a] -> [(a,a)]
combinations [] = []
combinations (a:as) =
  map (a,) as ++ combinations as


data Vec3 a = Vec3 a a a
  deriving (Eq, Ord, Show)

toVec3 :: [a] -> Vec3 a
toVec3 [x,y,z] = Vec3 x y z
toVec3 _ = undefined

instance Functor Vec3 where
  fmap f = toVec3 . map f . toList

instance Foldable Vec3 where
  foldMap f (Vec3 x y z) = foldMap f [x,y,z]
  length _ = 3

instance Num a => Num (Vec3 a) where
  (+) = toVec3 .* zipWith (+) `on` toList
  (*) = toVec3 .* zipWith (*) `on` toList
  abs = fmap abs
  signum = fmap signum
  negate = fmap negate
  fromInteger n = fromInteger <$> Vec3 n n n

data Vec2 a = Vec2 a a
  deriving (Eq, Ord, Show)

toVec2 :: [a] -> Vec2 a
toVec2 [x,y] = Vec2 x y
toVec2 _ = undefined

instance Functor Vec2 where
  fmap f = toVec2 . map f . toList

instance Foldable Vec2 where
  foldMap f (Vec2 x y) = foldMap f [x,y]
  length _ = 2

instance Num a => Num (Vec2 a) where
  (+) = toVec2 .* zipWith (+) `on` toList
  (*) = toVec2 .* zipWith (*) `on` toList
  abs = fmap abs
  signum = fmap signum
  negate = fmap negate
  fromInteger n = fromInteger <$> Vec2 n n

rotateLeft90 :: Num a => Vec2 a -> Vec2 a
rotateLeft90 (Vec2 x y) = Vec2 y (-x)

rotateRight90 :: Num a => Vec2 a -> Vec2 a
rotateRight90 (Vec2 x y) = Vec2 (-y) x

assertM :: Monad m => Bool -> m ()
assertM condition
  | condition = return ()
  | otherwise = error "assertion failure"

maximaBy :: forall t a. Foldable t => (a -> a -> Ordering) -> t a -> [a]
maximaBy comp = foldr go []
  where
    go :: a -> [a] -> [a]
    go a [] = [a]
    go a (m:ms) =
      case a `comp` m of
        EQ -> a:m:ms -- a also maximal ==> include
        LT -> m:ms   -- a not maximal ==> don't include
        GT -> [a]    -- a is greater ==> reject previous results

iterateJust :: (a -> Maybe a) -> a -> [a]
iterateJust f a = 
  case f a of
    Nothing -> [a]
    Just a' -> a : iterateJust f a'
