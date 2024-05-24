module DFA 
  ( DFA 
  ) where
import Prelude hiding (concat)
import Data.IntSet (IntSet)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.IntSet as IntSet
import Data.Containers.ListUtils (nubInt)
import Control.Monad (guard)
import RegExp (RegExp(..))

data DFA = DFA
  { startState  :: Int -- double serves as maximum state
  , finalStates :: IntSet
  , transitions :: Map (Int, Char) Int
  }

states :: DFA -> [Int]
states dfa = nubInt $ map fst (Map.keys dfa.transitions) ++ Map.elems dfa.transitions

renameStates :: (Int -> Int) -> DFA -> DFA
renameStates f dfa =
  DFA (f dfa.startState) (IntSet.map f dfa.finalStates) $ Map.fromList $ do
    ((state_from, trans), state_to) <- Map.toList dfa.transitions
    return ((f state_from, trans), f state_to)

{-| 
  Bijective encoding of a pair of integers as a single integer.
 
  >>> [ encodePair x y | x <- [0..3], y <- [0..3] ]
-}
encodePair :: Int -> Int -> Int
encodePair x y = ((x+y+1)*(x+y)) `div` 2 + y

intersection :: DFA -> DFA -> DFA
intersection dfa1 dfa2 = 
  let
    -- NOTE: does not account for disconnected components in dfas
    cross_product :: Map (Int, Char) Int
    cross_product = Map.fromList $ do
      ((from1, trans1), to1) <- Map.toList dfa1.transitions
      ((from2, trans2), to2) <- Map.toList dfa2.transitions
      guard $ trans1 == trans2
      return 
        ( (encodePair from1 from2, trans1)
        , encodePair to1 to2
        )

    -- TODO: test that this still satisfies invariant that start state has maximum value.
    start_state :: Int
    start_state = encodePair dfa1.startState dfa2.startState

    final_states :: IntSet
    final_states = IntSet.fromList $ do
      final1 <- IntSet.toList dfa1.finalStates
      final2 <- IntSet.toList dfa2.finalStates
      return $ encodePair final1 final2
  in
    DFA start_state final_states cross_product

union :: DFA -> DFA -> DFA
union dfa1 dfa2 = error "TODO"

concat :: DFA -> DFA -> DFA
concat dfa1 dfa2 = error "TODO"

star :: DFA -> DFA
star dfa = error "TODO"

fromRegExp :: RegExp -> DFA
fromRegExp = \case
  Empty          -> DFA 0 IntSet.empty Map.empty 
  EmptyString    -> DFA 0 (IntSet.singleton 0) Map.empty 
  Char lit       -> DFA 1 (IntSet.singleton 0) (Map.singleton (1, lit) 0) 
  Concat re1 re2 -> fromRegExp re1 `concat` fromRegExp re2
  Union  re1 re2 -> fromRegExp re1 `union` fromRegExp re2
  Star re        -> star $ fromRegExp re
