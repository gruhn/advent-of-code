{-# LANGUAGE OverloadedStrings #-}

module Main where
import Data.ByteString (ByteString)
import Crypto.Hash (MD5(MD5), hashWith, Digest)
import Data.String (fromString)
import qualified Data.List as L
import Control.Arrow (second)
import Data.Foldable (for_)

convolute :: Int -> [a] -> [[a]]
convolute window_size as 
  | length as < window_size = []
  | otherwise = take window_size as : convolute window_size (tail as)

toByteString :: String -> ByteString
toByteString = fromString

hashes :: ByteString -> [(Int, String)]
hashes salt = hash_with <$> [0..]
  where
    hash_with :: Int -> (Int, String)
    hash_with index = (index, show . hashWith MD5 . add_salt $ index)

    add_salt :: Int -> ByteString
    add_salt i = salt <> (toByteString . show $ i)

allEqual :: Eq a => [a] -> Bool
allEqual [] = True 
allEqual (a:as) = all (a ==) as

filterHashes :: [(Int, String)] -> [(Int, String)]
filterHashes []     = []
filterHashes ((index, hash) : rest)
  | keep_hash = (index, hash) : filterHashes rest
  | otherwise = filterHashes rest
  where
    find_triple = L.find allEqual . convolute 3
    contains_five_of triple = L.isInfixOf (replicate 5 $ head triple)
    keep_hash = 
      case find_triple hash of
        Nothing     -> False
        Just triple -> any (contains_five_of triple) (take 1000 $ snd <$> rest)

stretchedHash :: String -> String
stretchedHash = (!! 2016) . iterate (show . hashWith MD5 . toByteString)

main :: IO ()
main = do
  let salt = "ihaygndm"
      hash_stream = hashes salt

  putStrLn "Part 1: "
  let hashes_part1 = take 64 . filterHashes $ hash_stream
  for_ hashes_part1 print

  putStrLn "Part 2: "
  let hashes_part2 = take 64 . filterHashes . fmap (second stretchedHash) $ hash_stream
  for_ hashes_part2 print