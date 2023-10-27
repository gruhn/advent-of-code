module Main where
import Text.Megaparsec.Char (char)
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char.Lexer (decimal)
import Utils (parse)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Foldable (Foldable(foldl'))
import Data.Char (ord)
import Data.Bits (xor)
import Numeric (showHex)
import Prelude hiding (seq)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

rotate :: Int -> Seq Int -> Seq Int
rotate n seq = 
 let (prefix, suffix) = Seq.splitAt (n `mod` length seq) seq
  in suffix <> prefix

reversePrefix :: Int -> Seq Int -> Seq Int
reversePrefix len seq =
 let (prefix, suffix) = Seq.splitAt len seq
  in Seq.reverse prefix <> suffix

step :: Seq Int -> (Int, Int) -> Seq Int
step seq (shift, len) = rotate shift $ reversePrefix len seq

run :: [Int] -> Seq Int
run lengths = undo_shifts final_seq
 where
  skip_sizes = [0..]
  shifts = zipWith (+) skip_sizes lengths

  start_seq = Seq.fromList [0..255]
  final_seq = foldl' step start_seq (zip shifts lengths)

  undo_shifts = rotate (- sum shifts)

toHex :: Int -> Text
toHex = Text.justifyRight 2 '0' . Text.pack . (`showHex` "")

main :: IO ()
main = do
 input <- TextIO.readFile "input/10.txt"

 putStr "Part 1: "
 let lengths_p1 = parse (decimal `sepBy` char ',') input
 print $ product $ Seq.take 2 $ run lengths_p1

 putStr "Part 2: "
 let input_ascii = map ord $ Text.unpack input
     lengths_p2 = concat $ replicate 64 $ input_ascii <> [17, 31, 73, 47, 23]
     sparse_hash = run lengths_p2
     dense_hash = foldr1 xor <$> Seq.chunksOf 16 sparse_hash
     knot_hash = foldMap toHex dense_hash
 print knot_hash
