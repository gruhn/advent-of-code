{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Main where

import Utils (parseHardError, Parser)
import Text.Megaparsec.Char (string, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Control.Applicative ((<|>))
import Text.Megaparsec (sepBy)
import qualified Data.Set as S
import Data.Foldable (foldl')

data Instr = Rect Int Int | RotateRow Int Int | RotateCol Int Int
    deriving Show

parser :: Parser [Instr]
parser = instr `sepBy` newline
  where
    instr :: Parser Instr
    instr = rect <|> rotate

    rect :: Parser Instr
    rect = do
      string "rect "
      width <- decimal
      string "x"
      height <- decimal
      return $ Rect width height

    rotate :: Parser Instr
    rotate = string "rotate " *> (rotate_row <|> rotate_col)

    rotate_row :: Parser Instr
    rotate_row = do
      string "row y="
      row <- decimal
      string " by "
      shift <- decimal
      return $ RotateRow row shift

    rotate_col :: Parser Instr
    rotate_col = do
      string "column x="
      col <- decimal
      string " by "
      shift <- decimal
      return $ RotateCol col shift

data Display = Display
  { pixels :: S.Set (Int,Int)
  , width :: Int
  , height :: Int
  }

instance Show Display where
  show (Display pixels width height) = unlines rows
    where
      pixel_char xy
        | xy `S.member` pixels = '#'
        | otherwise            = ' '

      row y = [ pixel_char (x,y) | x <- [0..width-1] ]
      rows  = [ row y | y <- [0..height-1] ]

applyInstr :: Instr -> Display -> Display
applyInstr (Rect w h) display = display { pixels = S.union rect_pixels (pixels display) }
  where
    rect_pixels = S.fromList [ (x,y) | x <- [0..w-1], y <- [0..h-1] ]
applyInstr (RotateRow y shift) display = display { pixels = new_pixels }
  where
    new_pixels = S.union pixels_without_row pixels_in_row_shifted

    (pixels_in_row, pixels_without_row) = S.partition is_in_row (pixels display)
      where
        is_in_row (_, y') = y' == y

    pixels_in_row_shifted = S.map shift_pixel pixels_in_row
      where
        shift_pixel :: (Int,Int) -> (Int,Int)
        shift_pixel (x',y') = ((x' + shift) `mod` width display, y')
applyInstr (RotateCol x shift) display = display { pixels = new_pixels }
  where
    new_pixels = S.union pixels_without_col pixels_in_col_shifted

    (pixels_in_col, pixels_without_col) = S.partition is_in_col (pixels display)
      where
        is_in_col (x', _) = x' == x

    pixels_in_col_shifted = S.map shift_pixel pixels_in_col
      where
        shift_pixel :: (Int,Int) -> (Int,Int)
        shift_pixel (x',y') = (x', (y' + shift) `mod` height display)

main :: IO ()
main = do
  instructions <- parseHardError parser <$> readFile "input/08.txt"

  let display_initial = Display mempty 50 6
      display_final   = foldl' (flip applyInstr) display_initial instructions

  putStr "Part 1: "
  print $ S.size . pixels $ display_final

  putStrLn "Part 2: "
  print display_final