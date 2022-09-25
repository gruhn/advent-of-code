{-# LANGUAGE OverloadedStrings #-}
module Main where

import Crypto.Hash (hashlazy, MD5, Digest)
import Data.ByteString.Lazy (ByteString)
import Data.String (fromString)
import Data.Foldable (for_, find)
import Control.Monad (guard)
import Data.Char (digitToInt)
import Data.List (sortOn, delete)

hash :: ByteString -> Int -> Digest MD5
hash prefix suffix = 
    hashlazy (prefix <> fromString (show suffix))

hashesWithLeadingZeros :: Int -> ByteString -> [String]
hashesWithLeadingZeros n prefix =
  filter hasLeadingZeros $ show . hash prefix <$> [0..]
  where
    hasLeadingZeros :: String -> Bool
    hasLeadingZeros = (>= n) . length . takeWhile (=='0')

main :: IO ()
main = do 
  let hashes = hashesWithLeadingZeros 5 "cxdnnyjw"

  putStr "Part 1: "
  print $ (!! 5) <$> take 8 hashes

  putStr "Part 2: "
  let matches :: [(Int,Char)]
      matches = do
        hash <- hashes
        let (pos, char) = (digitToInt $ hash !! 5, hash !! 6)
        guard (0 <= pos && pos <= 7)
        return (pos, char)

      accum :: [Int] -> [(Int,Char)] -> [(Int,Char)]
      accum []      _  = []
      accum missing [] = []
      accum missing ((pos,char):rest) 
        | pos `elem` missing = (pos,char) : accum (pos `delete` missing) rest
        | otherwise          = accum missing rest

  print $ fmap snd $ sortOn fst $ accum [0..7] matches