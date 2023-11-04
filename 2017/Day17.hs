module Main where
import qualified Data.Sequence as Seq
import Data.Foldable (foldl')

stepSize :: Int
stepSize = 377

step :: (Int, Int) -> (Int, Int)
step (pos, i) = ((pos+stepSize) `mod` (i+1) + 1, i+1)

main :: IO ()
main = do 
  putStr "Part 1: "
  print
    $ (`Seq.index` 1)
    $ Seq.dropWhileL (/= 2017)
    $ foldl' (\buffer (pos, len) -> Seq.insertAt pos len buffer) Seq.empty
    $ take (2017+1)
    $ iterate step (0, 0)

  putStr "Part 2: "
  print 
    $ snd . last
    $ filter ((==1) . fst)
    $ take (50000000+1)
    $ iterate step (0, 0)
