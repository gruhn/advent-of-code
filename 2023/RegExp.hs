{-# LANGUAGE LambdaCase #-}
module RegExp where
import Prelude hiding (concat)
import Data.IntSet (IntSet)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.IntSet as IntSet

data RegExp
  = Empty
  | EmptyString
  | Literal Char 
  | Concat RegExp RegExp 
  | Union RegExp RegExp 
  | Star RegExp

instance Show RegExp where
  show = \case
    Empty -> "∅" 
    EmptyString -> "ε"
    Literal lit -> [lit]
    Concat re1 re2 -> show re1 ++ show re2
    Union re1 re2 -> "(" ++ show re1 ++ "|" ++ show re2 ++ ")"
    Star re -> "(" ++ show re ++ ")*"

{- concat :: RegExp -> RegExp -> RegExp
concat re1 re2 = 
  case (re1, re2) of
    (Empty      , re2)         -> Empty
    (re1        , Empty)       -> Empty
    (EmptyString, re2)         -> re2
    (re1        , EmptyString) -> re1
    (re1        , re2)         -> Concat re1 re2

union :: RegExp -> RegExp -> RegExp
union re1 re2 = 
  case (re1, re2) of
    (Empty, re2)   -> re1
    (re1  , Empty) -> re2
    (re1  , re2)   -> Union re1 re2 -}

-- intersection :: RegExp -> RegExp -> RegExp
-- intersection re1 re2 = 
--   case (re1, re2) of
--     (Empty, _) -> Empty
--     (_, Empty) -> Empty

--     (Union re11 re12, _) -> 
--       intersection re11 re2 `union` intersection re12 re2
--     (_, Union re21 re22) -> 
--       intersection re1 re21 `union` intersection re2 re22

--     (Star _   , EmptyString) -> EmptyString
--     (Star re11, Literal lit) -> _
--     (Star re11, Concat re21 re22) -> _
--     (Star re11, Star re21) -> _

--     (EmptyString, EmptyString) -> EmptyString
--     (EmptyString, Literal _) -> Empty
--     (EmptyString, Concat re21 re22) -> error "TODO"
--     (EmptyString, Star _) -> EmptyString
--     (_, EmptyString) -> intersection EmptyString re1

--     (Literal lit, EmptyString) -> EmptyString
--     (Literal lit, Star re) -> EmptyString

data NFA = NFA
  { getStartState  :: Int -- double serves as maximum state
  , getFinalStates :: IntSet
  , getTransitions :: Map (Int, Maybe Char) Int
  }

renameStates :: (Int -> Int) -> NFA -> NFA
renameStates f (NFA start_state final_states transitions) =
  NFA (f start_state) (IntSet.map f final_states) $ Map.fromList $ do
    ((state_from, trans), state_to) <- Map.toList transitions
    return ((f state_from, trans), f state_to)

union :: NFA -> NFA -> NFA
union nfa1 nfa2 =
  let
  in
    _

concat :: NFA -> NFA -> NFA
concat (NFA start1 finals1 trans1) nfa2 = 
  let
    NFA start2 finals2 trans2 = renameStates (+ (start1+1)) nfa2
    bridges = Map.fromList $ do
      final1 <- IntSet.toList finals1
      return ((final1, Nothing), start2)
  in
    NFA start1 finals2 $ Map.unions [trans1, trans2, bridges]

star :: NFA -> NFA
star (NFA start_state final_states transitions) =
  let
    new_transitions :: Map (Int, Maybe Char) Int
    new_transitions = Map.fromList $ do
      final <- IntSet.toList final_states
      return ((final, Nothing), start_state)
  in
    NFA 
      start_state 
      (IntSet.insert start_state final_states)
      (Map.union transitions new_transitions)

toNFA :: RegExp -> NFA
toNFA = \case
  Empty          -> NFA 0 IntSet.empty Map.empty 
  EmptyString    -> NFA 0 (IntSet.singleton 0) Map.empty 
  Literal lit    -> NFA 1 (IntSet.singleton 0) (Map.singleton (1, Just lit) 0) 
  Concat re1 re2 -> toNFA re1 `concat` toNFA re2
  Union  re1 re2 -> toNFA re1 `union` toNFA re2
  Star re        -> star $ toNFA re

fromNFA :: NFA -> RegExp
fromNFA = _
