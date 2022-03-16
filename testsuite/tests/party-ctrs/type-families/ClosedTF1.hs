{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeConstructors #-}

module ClosedTF1 where

-- import GHC.Types (Type)

data Ord a => Tree a = Leaf a | Node (Tree a) (Tree a)

type family F a where
  F [a] = a
  F a = Tree a

f :: a -> F a
f = undefined


