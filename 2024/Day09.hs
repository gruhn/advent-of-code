module Main (main) where

import Utils (Parser, parseFile)
import Data.Char (digitToInt)
import Data.Sequence (Seq ((:<|), (:|>)))
import qualified Data.Sequence as Seq
import Control.Applicative ((<|>), Alternative (many))
import Text.Megaparsec.Char (newline, digitChar)

data File = File 
  { size      :: Int
  , freeSpace :: Int
  , blockId   :: Int 
  } deriving Show

parser :: Parser [File]
parser = add_block_ids <$> many file
  where
    digit :: Parser Int
    digit = digitToInt <$> digitChar

    free_space :: Parser Int
    free_space = digit <|> (0 <$ newline)

    file :: Parser (Int -> File)
    file = File <$> digit <*> free_space

    add_block_ids :: [Int -> File] -> [File]
    add_block_ids files = zipWith ($) files [0..]

data Fitting = SplitFit File File | FullFit File | NoFit

fitInto :: File -> Int -> Fitting
fitInto file free_space
  | free_space == 0         = NoFit
  | free_space >= file.size = FullFit (file { freeSpace = free_space - file.size })
  | otherwise               = SplitFit file_fit file_rest
  where
    file_fit  = file { size = free_space, freeSpace = 0 }
    file_rest = file { size = file.size - free_space }

fill1 :: Seq File -> Seq File
fill1 Seq.Empty = Seq.Empty
fill1 (first_file :<| Seq.Empty) = first_file :<| Seq.Empty
fill1 (first_file :<| (rest_files :|> last_file)) = 
  first_file { freeSpace = 0 } :<| 
    case last_file `fitInto` first_file.freeSpace of
      NoFit -> 
        fill1 (rest_files :|> last_file)
      FullFit last_file_fit -> 
        fill1 (last_file_fit :<| rest_files)
      SplitFit last_file_fit last_file_rest -> 
        last_file_fit :<| fill1 (rest_files :|> last_file_rest)

fill2 :: Seq File -> Seq File
fill2 files = 
  let
    go :: Int -> Seq File -> Seq File
    go max_block_id Seq.Empty = Seq.Empty
    go max_block_id (rest_files :|> last_file) =
      if last_file.blockId >= max_block_id then
        go max_block_id rest_files :|> last_file
      else
        case try_full_fit last_file rest_files of
          Nothing -> 
            go last_file.blockId rest_files :|> last_file
          Just Seq.Empty -> 
            error "try_full_fit should not succeed with an empty sequence"
          Just (rest' :|> file') -> 
            go last_file.blockId $ rest' :|> file' { freeSpace = file'.freeSpace + last_file.size + last_file.freeSpace }

    try_full_fit :: File -> Seq File -> Maybe (Seq File)
    try_full_fit file Seq.Empty = Nothing
    try_full_fit file (first_file :<| rest_files) = 
      case file `fitInto` first_file.freeSpace of
        FullFit file_fit -> do
          return $ first_file { freeSpace = 0 } :<| file_fit :<| rest_files
        _ -> do 
          rest_files_modified <- try_full_fit file rest_files
          return $ first_file :<| rest_files_modified
  in
    go maxBound files

toBlocks :: File -> [Int]
toBlocks file = replicate file.size file.blockId ++ replicate file.freeSpace 0

checksum :: Seq File -> Int
checksum = sum . zipWith (*) [0..] . concatMap toBlocks

main :: IO ()
main = do
  input <- Seq.fromList <$> parseFile parser "input/09.txt"

  putStr "Part 1: "
  print $ checksum $ fill1 input

  putStr "Part 2: "
  print $ checksum $ fill2 input
