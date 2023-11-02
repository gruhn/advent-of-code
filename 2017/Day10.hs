module Main where
import Text.Megaparsec.Char (char)
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char.Lexer (decimal)
import Utils (parse)
import qualified Data.Sequence as Seq
import qualified Data.Text.IO as TextIO
import KnotHash (tieKnots, knotHash)

main :: IO ()
main = do
 input <- TextIO.readFile "input/10.txt"

 putStr "Part 1: "
 let lengths = parse (decimal `sepBy` char ',') input
 print $ product $ Seq.take 2 $ tieKnots lengths

 putStr "Part 2: "
 print $ knotHash input
