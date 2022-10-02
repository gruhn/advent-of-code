{-# LANGUAGE OverloadedStrings  #-}

module Main where
import Utils (Parser, parseHardError)
import Text.Megaparsec (choice, sepBy, (<|>), optional)
import Text.Megaparsec.Char (newline, string, lowerChar, letterChar, char)
import Text.Megaparsec.Char.Lexer (decimal)
import qualified Data.Text as T
import Data.Text (Text)

data Op = 
    SwapPos Int Int
  | SwapChar Char Char
  | RotateLeftBy Int 
  | RotateRightBy Int 
  | RotateRightTo Char
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
        rotate_to = RotateRightTo
          <$ string "based on position of letter " <*> letterChar 

    reverse :: Parser Op
    reverse = Reverse 
      <$ string "reverse positions " <*> decimal
      <* string " through "          <*> decimal

    move :: Parser Op
    move = Move
      <$ string "move position " <*> decimal
      <* string " to position "  <*> decimal

-- swap position X with position Y means that the letters at indexes X and Y (counting from 0) should be swapped.
-- swap letter X with letter Y means that the letters X and Y should be swapped (regardless of where they appear in the string).
-- rotate left/right X steps means that the whole string should be rotated; for example, one right rotation would turn abcd into dabc.
-- rotate based on position of letter X means that the whole string should be rotated to the right based on the index of letter X (counting from 0) as determined before this instruction does any rotations. Once the index is determined, rotate the string to the right one time, plus a number of times equal to that index, plus one additional time if the index was at least 4.
-- reverse positions X through Y means that the span of letters at indexes X through Y (including the letters at X and Y) should be reversed in order.
-- move position X to position Y means that the letter which is at index X should be removed from the string, then inserted such that it ends up at index Y.

-- apply :: Text -> Op -> Text
-- apply str (SwapPos x y) = undefined 

password :: Text
password = "abcdefgh"

main :: IO ()
main = do
  ops <- parseHardError parser <$> readFile "2016/input/21.txt"

  putStr "Part 1: "
  print ops