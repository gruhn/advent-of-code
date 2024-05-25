module NFA
  ( intersection
  , states
  ) where
import Prelude hiding (concat)
import Data.IntSet (IntSet)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.IntSet as IntSet
import Data.Containers.ListUtils (nubInt)

data NFA = NFA
  { startState  :: Int -- double serves as maximum state
  , finalStates :: IntSet
  , transitions :: Map (Int, Maybe Char) Int
  }

states :: NFA -> [Int]
states nfa = nubInt $ map fst (Map.keys nfa.transitions) ++ Map.elems nfa.transitions

renameStates :: (Int -> Int) -> NFA -> NFA
renameStates f nfa =
  NFA (f nfa.startState) (IntSet.map f nfa.finalStates) $ Map.fromList $ do
    ((state_from, trans), state_to) <- Map.toList nfa.transitions
    return ((f state_from, trans), f state_to)

union :: NFA -> NFA -> NFA
union nfa1 nfa2 =
  let
    max_state1 = nfa1.startState + 1
    nfa2' = renameStates (+ max_state1) nfa2
    new_start_state = nfa2'.startState + 1

    new_transitions :: Map (Int, Maybe Char) Int
    new_transitions = Map.fromList 
      [ ((new_start_state, Nothing), nfa1.startState)
      , ((new_start_state, Nothing), nfa2'.startState) 
      ]
  in
    NFA
      new_start_state 
      (IntSet.union nfa1.finalStates nfa2'.finalStates)
      (Map.unions [new_transitions, nfa1.transitions, nfa2'.transitions])

concat :: NFA -> NFA -> NFA
concat nfa1 nfa2 = 
  let
    max_state1 = nfa1.startState + 1
    nfa2' = renameStates (+ max_state1) nfa2
    bridge_transitions = Map.fromList $ do
      final1 <- IntSet.toList nfa1.finalStates
      return ((final1, Nothing), nfa2'.startState)
  in
    NFA nfa1.startState nfa2.finalStates 
      $ Map.unions [ nfa1.transitions, nfa2'.transitions, bridge_transitions ]

star :: NFA -> NFA
star nfa =
  let
    new_transitions :: Map (Int, Maybe Char) Int
    new_transitions = Map.fromList $ do
      final <- IntSet.toList nfa.finalStates
      return ((final, Nothing), nfa.startState)
  in
    NFA 
      nfa.startState
      (IntSet.insert nfa.startState nfa.finalStates)
      (Map.union nfa.transitions new_transitions)

{-| 
  Bijective encoding of a pair of integers as a single integer.
 
  >>> [ encodePair x y | x <- [0..3], y <- [0..3] ]
-}
encodePair :: Int -> Int -> Int
encodePair x y = ((x+y+1)*(x+y)) `div` 2 + y

intersection :: NFA -> NFA -> NFA
intersection nfa1 nfa2 = 
  let
    -- NOTE: does not account for disconnected components in NFAs
    cross_product :: Map (Int, Maybe Char) Int
    cross_product = Map.fromList $ do
      ((from1, trans1), to1) <- Map.toList nfa1.transitions
      ((from2, trans2), to2) <- Map.toList nfa2.transitions
      let from = encodePair from1 from2
          to   = encodePair to1   to2
      case (trans1, trans2) of
        (Nothing, _) -> 
          return ((from, Nothing), to)
        (_, Nothing) -> 
          return ((from, Nothing), to)
        (Just char1, Just char2) | char1 == char2 -> 
          return ((from, Just char1), to)
        (Just _, Just _) -> []

    -- TODO: test that this still satisfies invariant that start state has maximum value.
    start_state :: Int
    start_state = encodePair nfa1.startState nfa2.startState

    final_states :: IntSet
    final_states = IntSet.fromList $ do
      final1 <- IntSet.toList nfa1.finalStates
      final2 <- IntSet.toList nfa2.finalStates
      return $ encodePair final1 final2
  in
    NFA start_state final_states cross_product

toNFA :: RegExp -> NFA
toNFA = \case
  Empty          -> NFA 0 IntSet.empty Map.empty 
  EmptyString    -> NFA 0 (IntSet.singleton 0) Map.empty 
  Char lit       -> NFA 1 (IntSet.singleton 0) (Map.singleton (1, Just lit) 0) 
  Concat re1 re2 -> toNFA re1 `concat` toNFA re2
  Union  re1 re2 -> toNFA re1 `union` toNFA re2
  Star re        -> star $ toNFA re

fromNFA :: NFA -> RegExp
fromNFA = undefined
