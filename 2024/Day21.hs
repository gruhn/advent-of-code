module Main (main) where

import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (guard)
import Data.List (group, maximumBy, isPrefixOf)
import Data.Ord (comparing)
import Data.Maybe (catMaybes, maybeToList)
import Algorithm.Search (dijkstra)
import Utils (minimaBy)

-- data Node = Node Char Char Int
--   deriving (Eq, Ord, Show)

-- data Symbol = NonTerm Node | Term Char

-- type RuleSet = Map Node [[Symbol]]

type Keypad = Map Char (Map Char Char)

mkEdge :: Char -> Char -> Char -> (Char, Map Char Char)
mkEdge source dir target = (source, Map.singleton dir target)

keypadRow :: [Char] -> Keypad
keypadRow row = Map.fromListWith Map.union $ do
  (left, right) <- zip row (tail row)
  [ mkEdge left '>' right, mkEdge right '<' left ]

keypadCol :: [Char] -> Keypad
keypadCol col = Map.fromListWith Map.union $ do
  (top, bottom) <- zip col (tail col)
  [ mkEdge top 'v' bottom, mkEdge bottom '^' top ]

{- 
  +---+---+---+
  | 7 | 8 | 9 |
  +---+---+---+
  | 4 | 5 | 6 |
  +---+---+---+
  | 1 | 2 | 3 |
  +---+---+---+
      | 0 | A |
      +---+---+ 
-}
numericKeypad :: Keypad
numericKeypad =
  Map.unionsWith Map.union
    [ keypadRow "789"
    , keypadRow "456"
    , keypadRow "123"
    , keypadRow "0A"
    , keypadCol "741"
    , keypadCol "8520"
    , keypadCol "963A"
    ]

{- 
      +---+---+
      | ^ | A |
  +---+---+---+
  | < | v | > |
  +---+---+---+ 
-}
directionalKeypad :: Keypad
directionalKeypad =
  Map.unionsWith Map.union
    [ keypadRow "^A"
    , keypadRow "<v>"
    , keypadCol "<"
    , keypadCol "^v"
    , keypadCol "A>"
    ]

type State = ([Char], String)

move :: Char -> State -> Maybe State
move dir ([], _) = error "move: []"
move dir ([pos], output) = 
  case Map.lookup pos numericKeypad of
    Nothing      -> error "move -> numericKeypad -> Nothing"
    Just targets -> do 
      new_pos <- Map.lookup dir targets
      return ([new_pos], output)
move dir (pos : poses, output) = 
  case Map.lookup pos directionalKeypad of
    Nothing      -> error "move -> numericKeypad -> Nothing"
    Just targets -> do 
      new_pos <- Map.lookup dir targets
      return (new_pos : poses, output)

press :: State -> Maybe State
press ([], _) = error "press: []"
press ([pos], output) = Just ([pos], pos:output)
press (pos : poses, output) = do
  (poses', output') <- 
    if pos == 'A' then 
      press (poses, output)
    else 
      move pos (poses, output)
  return (pos : poses', output')

next :: String -> State -> [State]
next target state = presses ++ moves
  where
    moves :: [State]
    moves = catMaybes [ move dir state | dir <- "^v<>" ]

    presses :: [State]
    presses = do
      (poses, output) <- maybeToList $ press state
      guard $ reverse output `isPrefixOf` target
      return (poses, output)

-- neighbors :: Char -> Keypad -> [(Char, Char)]
-- neighbors source keypad =
--   case Map.lookup source keypad of
--     Nothing      -> error $ "missing: " ++ [source]
--     Just targets -> Map.toList targets

-- expand :: Node -> [[Symbol]]
-- expand (Node source target level) =
--   if source == target then
--     return []
--   else if level == 0 then do
--     (dir, neighbor) <- neighbors source numericKeypad
--     return 
--       [ NonTerm $ Node 'A' dir (level+1)
--       , Term 'A'
--       , NonTerm $ Node dir 'A' (level+1)
--       , Term 'A' 
--       , NonTerm $ Node neighbor target level
--       ]
--   else do
--     (dir, neighbor) <- neighbors source directionalKeypad
--     return 
--       [ NonTerm $ Node 'A' dir (level+1)
--       , Term 'A'
--       , NonTerm $ Node dir 'A' (level+1)
--       , Term 'A' 
--       , NonTerm $ Node neighbor target level 
--       ]
  
-- shortest :: String -> String -> String
-- shortest path1 path2
--   | length path1 < length path2 = path1
--   | length path1 > length path2 = path2
--   | otherwise = maximumBy (comparing score) [path1, path2]
--   where
--     score :: String -> Int
--     score = sum . map ((^2) . length) . group

-- transitiveClosure :: Keypad -> Keypad
-- transitiveClosure keypad =
--   let
--     transitive_paths :: Keypad
--     transitive_paths = Map.fromListWith shortest $ do
--       ((src , mid), src_to_mid) <- Map.toList keypad
--       ((mid', trg), mid_to_trg) <- Map.toList keypad
--       if src == trg then
--         return ((src, src), "")
--       else do
--         guard $ mid' == mid
--         return ((src, trg), src_to_mid ++ mid_to_trg)

--     new_keypad :: Keypad
--     new_keypad = Map.unionWith shortest keypad transitive_paths
--   in
--     if keypad == new_keypad then
--       keypad
--     else
--       transitiveClosure new_keypad

-- numericKeypadPaths :: String -> [String]
-- numericKeypadPaths []  = return []
-- numericKeypadPaths [_] = return []
-- numericKeypadPaths (button1 : button2 : buttons) = do
--   step_path <- numericKeypad Map.! (button1, button2) 
--   rest_path <- numericKeypadPaths (button2 : buttons)
--   return $ step_path ++ "A" ++ rest_path

-- numericKeypadPath :: String -> String
-- numericKeypadPath buttons = do
--   step <- zip buttons (tail buttons)
--   numericKeypad Map.! step ++ "A"

-- directionalKeypadPath :: String -> String
-- directionalKeypadPath buttons = do
--   step <- zip buttons (tail buttons)
--   directionalKeypad Map.! step ++ "A"

-- newtype DirMap = DirMap (Map (Char, Char) Path)

-- data PathSegment = Unit String | Or String String

-- type Path = [PathSegment]

-- instance Semigroup DirMap where
--   DirMap edges1 <> DirMap edges2 = DirMap $ Map.map embed_each edges1
--     where
--       embed_each :: Path -> Path
--       embed_each = minimaBy (comparing length) . concatMap embed

--       embed :: Path -> Path
--       embed path = do
--         (source, target) <- zip ('A' : path) path
--         edges2 Map.! (source, target)
      
main :: IO ()
main = do
  codes <- lines <$> readFile "input/21.txt"

  print $ sum $ catMaybes $ do 
    code <- codes
    let start_state = ("AAA", "")
        cost _ _ = 1
        is_target (_, output) = reverse output == code 

        leading_nums :: Int
        leading_nums = read $ init code

    return $ do 
      (path_len, _) <- dijkstra (next code) cost is_target start_state
      return (path_len * leading_nums)

