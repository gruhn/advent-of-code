module Main (main) where

import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (guard)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes, maybeToList)

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

{- data PathSegment = Unit Char | Or [Char]

type Path = [PathSegment]

expand :: Path -> [String]
expand [] = return ""
expand (segment : path) = 
  case segment of
    Unit char -> do
      str <- expand path
      return (char:str)
    Or options -> do
      char <- options
      str  <- path
      return (char:str)
  
join :: PathSegment -> PathSegment -> PathSegment
join (Unit char1) (Unit char2)
  | char1 == char2 = Unit char1
  | otherwise      = Or [char1, char2]
join (Unit char1) (Or chars2) = Or (char1 : chars2)
join (Or chars1) (Unit char2) = Or (char2 : options)
join (Or chars1) (Or chars2) = Or (char2 : options) -}

-- data PathSegment = Unit Char | Swap Char Char
--   deriving (Eq, Ord, Show)

-- type Path = [PathSegment]

-- collapse :: String -> String -> Path
-- collapse str1 str2 = 
--   case (str1, str2) of
--     ([], []) -> 
--       []
--     (a:as, b:bs) | a == b -> 
--       Unit a : collapse as bs
--     (a1:a2:as, b1:b2:bs) | a1 == b2 && a2 == b1 -> 
--       Swap a1 a2 : collapse as bs
--     _ -> 
--       error $ "collapse: " ++ show (str1, str2)   

{- shortest :: Set String -> Set String -> Set String
shortest paths1 paths2 = 
  Set.fromList $ minimaBy (comparing length) $ Set.toList $ paths1 <> paths2

transitiveClosure :: Map (Char, Char) (Set String) -> Map (Char, Char) (Set String)
transitiveClosure keypad =
  let
    transitive_paths :: Map (Char, Char) (Set String)
    transitive_paths = Map.fromListWith shortest $ do
      ((src , mid), src_to_mids) <- Map.toList keypad
      ((mid', trg), mid_to_trgs) <- Map.toList keypad
      if src == trg then
        return ((src, src), Set.singleton "")
      else do
        guard $ mid' == mid
        src_to_mid <- Set.toList src_to_mids
        mid_to_trg <- Set.toList mid_to_trgs
        return ((src, trg), Set.singleton $ src_to_mid ++ mid_to_trg)

    new_keypad :: Map (Char, Char) (Set String)
    new_keypad = Map.unionWith shortest keypad transitive_paths
  in
    if keypad == new_keypad then
      keypad
    else
      transitiveClosure new_keypad -}

data Path = Empty | Unit Char | Concat Path Path | Or Path Path

pathLength :: Path -> Int
pathLength Empty  = 0
pathLength Unit _ = 1
pathLength (Concat front rear) = pathLength front + pathLength rear
pathLength (Or left right) = max (pathLength left) (pathLength right)

-- join :: Path -> Path -> Path
-- join path1 path2 = 
--   case (path1, path2) of
--     ([], []) -> 
--       []
--     (a:as, b:bs) | a == b -> 
--       a : join as bs
--     (Unit a1 : Unit a2 : as, Unit b1 : Unit b2 : bs) | a1 == b2 && a2 == b1 -> 
--       Swap a1 a2 : join as bs
--     (Unit a1 : Unit a2 : as, Swap b1 b2 : bs) | (a1 == b1 && a2 == b2) || (a1 == b2 && a2 == b1) ->
--       Swap b1 b2 : join as bs
--     (Swap a1 a2 : as, Unit b1 : Unit b2 : bs) | (a1 == b1 && a2 == b2) || (a1 == b2 && a2 == b1) ->
--       Swap b1 b2 : join as bs
--     (Swap a1 a2 : as, Swap b1 b2 : bs) | (a1 == b1 && a2 == b2) || (a1 == b2 && a2 == b1) ->
--       Swap b1 b2 : join as bs
--     _ -> 
--       error $ "join: " ++ show (path1, path2)   

shortest :: Path -> Path -> Path
shortest path1 path2
  | pathLength path1 < pathLength path2 = path1
  | pathLength path1 > pathLength path2 = path2
  | otherwise = join path1 path2

transitiveClosure :: Map (Char, Char) Path -> Map (Char, Char) Path
transitiveClosure keypad =
  let
    transitive_paths :: Map (Char, Char) Path
    transitive_paths = Map.fromListWith shortest $ do
      ((src , mid), src_to_mid) <- Map.toList keypad
      ((mid', trg), mid_to_trg) <- Map.toList keypad
      if src == trg then
        return ((src, src), Empty)
      else do
        guard $ mid' == mid
        return ((src, trg), Concat src_to_mid mid_to_trg)

    new_keypad :: Map (Char, Char) Path
    new_keypad = Map.unionWith shortest keypad transitive_paths
  in
    if keypad == new_keypad then
      keypad
    else
      transitiveClosure new_keypad

newtype DirMap = DirMap { getEdges :: Map (Char, Char) Path }
  deriving (Eq, Show)

instance Semigroup DirMap where
  DirMap edges1 <> DirMap edges2 = DirMap $ Map.map embed_each edges1
    where
      embed_each :: Path -> Path
      embed_each = foldr1 shortest . embed . (Unit 'A' `Concat`)

      embed :: Path -> Path
      embed Empty = Empty
      embed (Unit char) = Unit char
      embed (Concat front rear) = Concat
      embed (Or left right) = _ 

toDirMap :: Keypad -> DirMap
toDirMap keypad =
  DirMap $ Map.map (++ [Unit 'A']) $ transitiveClosure $ Map.fromList $ do
    (source, targets) <- Map.toList keypad
    (dir, target) <- Map.toList targets
    return ((source, target), [Unit dir])

main :: IO ()
main = do
  codes <- lines <$> readFile "input/21.txt"

  -- print $ sum $ catMaybes $ do 
  --   code <- codes
  --   let start_state = ("AAA", "")
  --       cost _ _ = 1
  --       is_target (_, output) = reverse output == code 

  --       leading_nums :: Int
  --       leading_nums = read $ init code

  --   return $ do 
  --     (path_len, _) <- dijkstra (next code) cost is_target start_state
  --     return (path_len * leading_nums)

  let dir_map = toDirMap directionalKeypad

  traverse_ print $ Map.toList $ getEdges $ dir_map <> dir_map
