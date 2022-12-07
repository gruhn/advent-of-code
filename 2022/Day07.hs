module Main where
import Utils (Parser, parseHardError)
import Text.Megaparsec (sepBy, noneOf, choice, sepEndBy)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char ( string, newline, char )
import Control.Applicative ( some, (<|>), many )
import qualified Data.Map as M
import Control.Arrow (first)
import Data.List (partition, sortOn, groupBy, stripPrefix)
import Data.Foldable (for_)

data File = File { bytes :: Int, fileName :: String }
  deriving Show

data Cmd = Cd String | Ls [File]
  deriving Show

parser :: Parser [Cmd]
parser = many cmd
  where
    file_name :: Parser String
    file_name = some (noneOf " \n")

    files :: Parser [File]
    files = (file <|> dir) `sepEndBy` newline

    file = File <$> decimal <* string " " <*> file_name
    dir = File 0 <$ string "dir " <*> file_name

    cmd :: Parser Cmd
    cmd = string "$ " *> (cd <|> ls)

    cd = Cd <$ string "cd " <*> file_name <* newline
    ls = Ls <$ string "ls\n" <*> files

type Path = [String]

dirSizes :: [Cmd] -> M.Map Path Int
dirSizes = M.mapKeys reverse . go []
  where
    is_direct_sub_dir :: Path -> Path -> Bool
    is_direct_sub_dir path (dir:path') = path == path'
    is_direct_sub_dir _ _ = False

    direct_sub_dirs :: Path -> M.Map Path Int -> M.Map Path Int
    direct_sub_dirs path = M.filterWithKey (\sub_path _ -> path `is_direct_sub_dir` sub_path)

    go :: Path -> [Cmd] -> M.Map Path Int
    go path [] = M.empty
    go path (cmd : cmds) = 
      case (cmd, path) of 
        (Cd "..", []) -> go [] cmds
        (Cd "..", dir:path') -> go path' cmds
        (Cd dir, _) -> go (dir:path) cmds
        (Ls files, _) -> M.insert path (file_sizes + dir_sizes) path_map 
          where
            path_map = go path cmds
            file_sizes = sum (bytes <$> files)
            dir_sizes = sum (direct_sub_dirs path path_map)

main :: IO ()
main = do
  input <- parseHardError parser <$> readFile "input/07.txt"

  let dir_sizes = dirSizes input

  putStr "Part 1: "
  print $ sum $ filter (<= 100000) $ M.elems dir_sizes

  putStr "Part 2: "
  let root_size = dir_sizes M.! ["/"]
      unused_space = 70000000 - root_size
      free_up_at_least = 30000000 - unused_space

  print $ minimum $ filter (>= free_up_at_least) $ M.elems dir_sizes