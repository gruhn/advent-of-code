{-# LANGUAGE OverloadedStrings #-}
module Main where

import Crypto.Hash (hashlazy, MD5, Digest)
import Data.ByteString.Lazy (ByteString)
import Data.String (fromString)
import Data.Foldable (for_, find)

hash :: ByteString -> Int -> Digest MD5
hash prefix suffix = 
    hashlazy (prefix <> fromString (show suffix))

leadingZeroCount :: Digest MD5 -> Int
leadingZeroCount =
    length . takeWhile (=='0') . show

main :: IO ()
main = do
    let secretKey = "bgvyzdsv"

    putStr "Part 1: "
    print $ find ((==5) . leadingZeroCount . hash secretKey) [1..]

    putStr "Part 2: "
    print $ find ((==6) . leadingZeroCount . hash secretKey) [1..]