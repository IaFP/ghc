
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeConstructors #-}

module ClosedTF1 where

import GHC.Types (Type)

data Ord a => Tree a = Leaf a | Node (Tree a) (Tree a)

type family F a where
  F [a] = Tree a
  F a = [a]

f :: a -> F a
f = undefined


