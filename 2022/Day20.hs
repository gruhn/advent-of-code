module Main where
import qualified Data.Sequence as S
import Data.Sequence (Seq)
import Data.Foldable (foldl', Foldable (toList), traverse_)
import Data.Maybe (fromJust)
import Data.List (elemIndex , delete)

parse :: String -> [Int]
parse = fmap read . lines

mix :: [Int] -> [Int]
mix xs = fmap snd $ toList $ foldl' go ps ps
  where
    ps = S.fromList (zip [0..] xs)

    go :: Seq (Int, Int) -> (Int, Int) -> Seq (Int, Int)
    go ps p@(_, x) = ps''
      where 
        old_index = fromJust $ S.elemIndexL p ps
        ps' = S.deleteAt old_index ps
        new_index = (old_index + x) `mod` length ps'
        ps'' = S.insertAt (if new_index == 0 then length ps' else new_index) p ps'

main :: IO ()
main = do
  input <- parse <$> readFile "input/20.txt"

  let result xs = sum $ do
        let index_0 = fromJust $ elemIndex 0 xs
        s <- [1000, 2000, 3000]
        return $ xs !! ((index_0+s) `mod` length xs)

  putStr "Part 1: "
  print $ result (mix input)

  putStr "Part 2: "
  print 
    $ result
    $ (!! 10)
    $ iterate mix 
    $ fmap (*811589153) input