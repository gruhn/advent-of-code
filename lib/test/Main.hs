module Main (main) where

import Test.QuickCheck (quickCheck)
import Graph 
  ( prop_clean_node_delete
  , prop_invariant_pre_suc_keyset_match
  , prop_node_contract
  )

main :: IO ()
main = do
  quickCheck prop_clean_node_delete
  -- quickCheck prop_invariant_pre_suc_keyset_match
  quickCheck prop_node_contract
