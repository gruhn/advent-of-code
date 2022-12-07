module Main where
import Utils (Parser, parseHardError)
import Text.Megaparsec (sepBy, noneOf, choice, sepEndBy)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char ( string, newline, char )
import Control.Applicative ( some, (<|>), many )
import qualified Data.Map as M
import Control.Arrow (first)
import Data.List (partition, sortOn, groupBy)
import Data.Foldable (for_)

data OutputLine =
    DirLine String
  | FileLine Int String
  deriving Show

data Cmd =
    Cd String
  | Ls [OutputLine]
  deriving Show

parser :: Parser [Cmd]
parser = many cmd
  where
    file_name :: Parser String
    file_name = some (noneOf " \n")

    output_lines :: Parser [OutputLine]
    output_lines = (file <|> dir) `sepEndBy` newline

    file = FileLine <$> decimal <* string " " <*> file_name
    dir  = DirLine <$ string "dir " <*> file_name

    cmd :: Parser Cmd
    cmd = string "$ " *> (cd <|> ls)

    cd = Cd <$ string "cd " <*> file_name <* newline
    ls = Ls <$ string "ls" <* newline <*> output_lines

data FileTree = Dir (M.Map String FileTree) | File Int
  deriving Show

type Path = [String]

collectFiles :: [Cmd] -> M.Map Path Int
collectFiles = M.mapKeys reverse . from_commands []
  where
    from_commands :: Path -> [Cmd] -> M.Map Path Int
    from_commands path [] = M.empty
    from_commands path (cmd : cmds) = 
      case (cmd, path) of
        (Cd "..", [])        -> from_commands [] cmds
        (Cd "..", dir:path') -> from_commands path' cmds
        (Cd dir, _)          -> from_commands (dir:path) cmds
        (Ls lines, _)        -> M.union (from_outputs path lines) (from_commands path cmds)

    from_outputs :: Path -> [OutputLine] -> M.Map Path Int
    from_outputs path lines = M.fromList $ do
      line <- lines
      case line of
        DirLine _ -> []
        FileLine size name -> [(name:path, size)]

singleton :: Path -> Int -> FileTree
singleton []         bytes = File bytes
singleton (dir:path) bytes = 
  Dir $ M.singleton dir $ singleton path bytes

merge :: FileTree -> FileTree -> FileTree
merge (File bytes) (File _) = File bytes -- file overwrite
merge (Dir files) (Dir files') = Dir $ M.unionWith merge files files'
merge _ _ = error "tried to insert file into non-directory"

fileTree :: M.Map Path Int -> FileTree
fileTree = foldr merge (Dir M.empty) . fmap (uncurry singleton) . M.toList

dirSizes :: FileTree -> M.Map [String] Int
dirSizes = M.mapKeys reverse . go []
  where
    is_file (File _) = True
    is_file _ = False

    get_size (File bytes) = bytes
    get_size _ = 0

    go :: [String] -> FileTree -> M.Map [String] Int
    go path (File _) = M.empty
    go path (Dir files) = M.insert path dir_size sizes'
      where
        sizes' = M.unionsWith (+) $ M.mapWithKey (\name dir -> go (name:path) dir) sub_dirs

        (sub_files, sub_dirs) = M.partition is_file files

        direct_child_sizes = (sizes' M.!) . (:path) <$> M.keys sub_dirs

        dir_size = sum direct_child_sizes + sum (get_size <$> sub_files)

main :: IO ()
main = do
  input <- parseHardError parser <$> readFile "input/07.txt"

  let dir_sizes = dirSizes $ fileTree $ collectFiles input

  for_ (M.toList dir_sizes) print

  putStr "Part 1: "
  print $ sum $ filter (<= 100000) $ M.elems dir_sizes

  putStr "Part 2: "
  let root_size = dir_sizes M.! ["/"]
      unused_space = 70000000 - root_size
      free_up_at_least = 30000000 - unused_space

  print $ minimum $ filter (>= free_up_at_least) $ M.elems dir_sizes