module KnotHash (tieKnots, knotHash) where
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Char (ord)
import Data.Bits (xor)
import Numeric (showHex)
import Data.Foldable (foldl')

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

tieKnots :: [Int] -> Seq Int
tieKnots lengths = undo_shifts final_seq
 where
  skip_sizes = [0..]
  shifts = zipWith (+) skip_sizes lengths

  start_seq = Seq.fromList [0..255]
  final_seq = foldl' step start_seq (zip shifts lengths)

  undo_shifts = rotate (- sum shifts)

toHex :: Int -> Text
toHex = Text.justifyRight 2 '0' . Text.pack . (`showHex` "")

sparseHash :: Text -> Seq Int
sparseHash input = sparse_hash
 where
  input_ascii = map ord $ Text.unpack input
  lengths = concat $ replicate 64 $ input_ascii <> [17, 31, 73, 47, 23]
  sparse_hash = tieKnots lengths

denseHash :: Seq Int -> Seq Int
denseHash sparse_hash = foldr1 xor <$> Seq.chunksOf 16 sparse_hash

knotHash :: Text -> Text
knotHash = foldMap toHex . denseHash . sparseHash
