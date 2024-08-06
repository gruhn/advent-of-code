module Zipper 
  ( Zipper
  , shiftLeft
  , shiftRight
  ) where

import Control.Monad (ap)
import Control.Comonad (Comonad)
import Control.Comonad.Store (extract)
import Control.Comonad.Identity (duplicate)

data Zipper a = Zipper [a] a [a]

shiftLeft :: Zipper a -> Zipper a
shiftLeft (Zipper _  _ [])     = undefined
shiftLeft (Zipper ls c (r:rs)) = Zipper (c:ls) r rs

shiftRight :: Zipper a -> Zipper a
shiftRight (Zipper []     _ _ ) = undefined
shiftRight (Zipper (l:ls) c rs) = Zipper ls l (c:rs)

instance Functor Zipper where
  fmap f (Zipper ls c rs) = Zipper (fmap f ls) (f c) (fmap f rs)

instance Applicative Zipper where
  pure c = Zipper (repeat c) c (repeat c)
  (<*>)= ap

instance Monad Zipper where
  return = pure
  zipper >>= f = fmap (extract . f) zipper

instance Comonad Zipper where
  extract   (Zipper _ c _)   = c
  duplicate zipper = 
    Zipper 
      (iterate shiftRight zipper) 
      zipper 
      (iterate shiftLeft zipper)
