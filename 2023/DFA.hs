module DFA 
  ( DFA 
  ) where

import Prelude hiding (concat)
import Data.IntSet (IntSet)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.IntSet as IntSet
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

data DFA = DFA
  { startState  :: Int -- double serves as maximum state
  , finalStates :: IntSet
  , transitions :: IntMap (Map Char Int)
  }

states :: DFA -> IntSet
states dfa = IntMap.keysSet dfa.transitions <> dfa.finalStates

data InfInt = NegInf | Fin Int | PosInf

instance Num InfInt where
  Fin x + Fin y = Fin (x+y)
  inf + Fin _ = inf
  Fin _ + inf = inf
  PosInf + PosInf = PosInf
  NegInf + NegInf = NegInf
  _ + _ = undefined

  Fin x * Fin y = Fin (x*y)
  Fin x * inf = 
    case signum x of
      0  -> Fin 0
      1  -> inf
      -1 -> negate inf
  inf * Fin y = Fin y * inf
  inf1 * inf2 

  abs (Fin x) = Fin (abs x)
  abs PosInf = PosInf
  abs NegInf = PosInf

  signum (Fin x) = Fin (signum x)
  signum PosInf = 1
  signum NegInf = -1

  fromInteger x = Fin (fromInteger x)

  negate (Fin x) = Fin (negate x)
  negate PosInf = NegInf
  negate NegInf = PosInf


wordCount :: DFA -> InfInt
wordCount _ = _

{-| 
  Bijective encoding of a pair of integers as a single integer:

     y\x | 0 1 2 3 4 
    -----+-----------
      0  | 0 1 3 6
      1  | 2 4 7
      2  | 5 8
      3  | 9  
      4  |  
 
  >>> [ encodePair x y | x <- [0..3], y <- [0..3] ]
-}
encodePair :: Int -> Int -> Int
encodePair x y = ((x+y+1)*(x+y)) `div` 2 + y

intersection :: DFA -> DFA -> DFA
intersection dfa1 dfa2 = 
  let
    -- NOTE: does not account for disconnected components in dfas
    cross_product :: IntMap (Map Char Int)
    cross_product = IntMap.fromList $ do
      (from1, trans1) <- IntMap.toList dfa1.transitions
      (from2, trans2) <- IntMap.toList dfa2.transitions
      let from = encodePair from1 from2
          trans = Map.intersectionWith encodePair trans1 trans2
      return (from, trans)

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

-- fromRegExp :: RegExp -> DFA
-- fromRegExp = \case
--   Empty          -> DFA 0 IntSet.empty Map.empty 
--   EmptyString    -> DFA 0 (IntSet.singleton 0) Map.empty 
--   Char lit       -> DFA 1 (IntSet.singleton 0) (Map.singleton (1, lit) 0) 
--   Concat re1 re2 -> fromRegExp re1 `concat` fromRegExp re2
--   Union  re1 re2 -> fromRegExp re1 `union` fromRegExp re2
--   Star re        -> star $ fromRegExp re
