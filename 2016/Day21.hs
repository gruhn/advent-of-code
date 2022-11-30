module Main where
import Utils (Parser, parseHardError)
import Text.Megaparsec (choice, sepBy, (<|>), optional)
import Text.Megaparsec.Char (newline, string, lowerChar, letterChar, char)
import Text.Megaparsec.Char.Lexer (decimal)
import qualified Data.Maybe as M
import Data.Foldable (foldl', Foldable (toList))
import qualified Data.Sequence as S

data Op = 
    SwapPos Int Int
  | SwapChar Char Char
  | RotateLeftBy Int 
  | RotateRightBy Int 
  | RotateLeftWith Char
  | RotateRightWith Char
  | Reverse Int Int
  | Move Int Int
  deriving Show

parser :: Parser [Op]
parser = op `sepBy` newline
  where
    op = choice [swap, rotate, reverse, move]

    swap :: Parser Op
    swap = string "swap " *> (swap_pos <|> swap_char)
      where
        swap_pos = SwapPos 
          <$ string "position "       <*> decimal 
          <* string " with position " <*> decimal
        swap_char = SwapChar
          <$ string "letter "       <*> letterChar
          <* string " with letter " <*> letterChar 

    rotate :: Parser Op
    rotate = string "rotate " *> (rotate_left <|> rotate_right <|> rotate_to)
      where
        steps = string " step" *> optional (char 's')

        rotate_left = RotateLeftBy
          <$ string "left " <*> decimal <* steps
        rotate_right = RotateRightBy
          <$ string "right " <*> decimal <* steps
        rotate_to = RotateRightWith
          <$ string "based on position of letter " <*> letterChar 

    reverse :: Parser Op
    reverse = Reverse 
      <$ string "reverse positions " <*> decimal
      <* string " through "          <*> decimal

    move :: Parser Op
    move = Move
      <$ string "move position " <*> decimal
      <* string " to position "  <*> decimal

rotate :: Int -> S.Seq a -> S.Seq a
rotate _ S.Empty = S.Empty
rotate n seq
  | n > 0     = rotate (n-1) (last S.<| init)
  | n < 0     = rotate (n+1) (tail S.|> head)
  | otherwise = seq
  where
    (head S.:< tail) = S.viewl seq
    (init S.:> last) = S.viewr seq

-- >>> slice 0 4 $ S.fromList "abcdefghi"
-- (fromList "",fromList "abcde",fromList "fghi")

slice :: Int -> Int -> S.Seq a -> (S.Seq a, S.Seq a, S.Seq a)
slice x y seq = (pref, inf, suff)
  where
    pref = S.take x seq
    inf  = S.drop x $ S.take (y+1) seq
    suff = S.drop (y+1) seq

apply :: S.Seq Char -> Op -> S.Seq Char
-- swap position X with position Y means that the letters at indexes X and Y 
-- (counting from 0) should be swapped.
apply str (SwapPos x y) = S.update x char_y . S.update y char_x $ str
  where
    char_x = S.index str x
    char_y = S.index str y
-- swap letter X with letter Y means that the letters X and Y should be swapped 
-- (regardless of where they appear in the string).
apply str (SwapChar char_x char_y) = apply str (SwapPos x y)
  where
    x = M.fromJust $ S.elemIndexL char_x str
    y = M.fromJust $ S.elemIndexL char_y str
-- rotate left/right X steps means that the whole string should be rotated; 
-- for example, one right rotation would turn abcd into dabc.
apply str (RotateLeftBy  n) = rotate (-n) str
apply str (RotateRightBy n) = rotate n str
-- rotate based on position of letter X means that the whole string should be rotated to 
-- the right based on the index of letter X (counting from 0) as determined before this 
-- instruction does any rotations. Once the index is determined, rotate the string to the 
-- right one time, plus a number of times equal to that index, plus one additional time 
-- if the index was at least 4.
apply str (RotateRightWith char) = rotate n str
  where
    x = M.fromJust $ S.elemIndexL char str
    n = if x >= 4 then x+2 else x+1
-- hacky custom inverse of `RotateRightWith`
apply str (RotateLeftWith char) =
  case S.elemIndexL char str of
    Just 0 -> rotate (-1) str
    Just 1 -> rotate (-1) str
    Just 2 -> rotate (-6) str
    Just 3 -> rotate (-2) str
    Just 4 -> rotate (-7) str
    Just 5 -> rotate (-3) str
    Just 6 -> rotate (-0) str
    Just 7 -> rotate (-4) str
    _ -> undefined
-- reverse positions X through Y means that the span of letters at indexes X through Y 
-- (including the letters at X and Y) should be reversed in order.
apply str (Reverse x y) = pref <> S.reverse inf <> suff
  where
    (pref, inf, suff) = slice x y str
-- move position X to position Y means that the letter which is at index X should be 
-- removed from the string, then inserted such that it ends up at index Y.
apply str (Move x y) = S.insertAt y (S.index str x) . S.deleteAt x $ str

inverse :: Op -> Op
inverse (SwapPos x y) = SwapPos x y
inverse (SwapChar c1 c2) = SwapChar c1 c2
inverse (RotateLeftBy n) = RotateRightBy n
inverse (RotateRightBy n) = RotateLeftBy n 
inverse (RotateRightWith c) = RotateLeftWith c
inverse (RotateLeftWith c) = RotateRightWith c
inverse (Reverse x y) = Reverse x y
inverse (Move x y) = Move y x

main :: IO ()
main = do
  ops <- parseHardError parser <$> readFile "input/21.txt"

  putStr "Part 1: "
  print $ foldl' apply (S.fromList "abcdefgh") ops

  putStr "Part 2: "
  let inverse_ops = reverse (inverse <$> ops)
  print $ foldl' apply (S.fromList "fbgdceah") inverse_ops