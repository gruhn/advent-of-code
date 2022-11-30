{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
{-# LANGUAGE TupleSections #-}
module Main where
import Utils (Parser, parseHardError)
import Text.Megaparsec (sepBy, between, some, (<|>))
import Text.Megaparsec.Char (newline, lowerChar, char)
import Data.List (partition)
import Data.Bifunctor (Bifunctor(bimap))
import Control.Monad (guard)

type IP = ([String], [String])

parser :: Parser [IP]
parser = ip `sepBy` newline
  where 
    naked_segment   = (,False) <$> some lowerChar
    bracket_segment = (,True)  <$> between (char '[') (char ']') (some lowerChar)

    partitionSegments = bimap (fmap fst) (fmap fst) . partition snd

    ip = partitionSegments <$>
      some (bracket_segment <|> naked_segment)

-- >>> convolute 3 "abcdef"
-- ["abc","bcd","cde"]

convolute :: Int -> [a] -> [[a]]
convolute window_size as 
  | length as < window_size = []
  | otherwise = take window_size as : convolute window_size (tail as)

isHeterogenePalindom :: Eq a => [a] -> Bool
isHeterogenePalindom as = 
  reverse as == as && as !! 0 /= as !! 1

hetPals :: Int -> [String] -> [String]
hetPals size segments = do 
  segment <- segments
  window <- convolute size segment 
  guard (isHeterogenePalindom window)
  return window

supportsTLS :: IP -> Bool
supportsTLS (bracket_segments, naked_segments) =
  (null . hetPals 4 $ bracket_segments) && (not . null . hetPals 4 $ naked_segments)

supportsSSL :: IP -> Bool
supportsSSL (bracket_segments, naked_segments) = not . null $ do
  aba <- hetPals 3 naked_segments
  bab <- hetPals 3 bracket_segments
  let [a,b,_] = aba
  guard (bab == [b,a,b])
  return aba

main :: IO ()
main = do 
  ips <- parseHardError parser <$> readFile "input/07.txt"

  putStr "Part 1: "
  print $ length $ filter supportsTLS ips 

  putStr "Part 2: "
  print $ length $ filter supportsSSL ips