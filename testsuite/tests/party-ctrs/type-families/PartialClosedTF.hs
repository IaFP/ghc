{-# LANGUAGE TypeFamilies #-}

module PartialClosedTF1 where

import GHC.Types (Type)

data Tree a = Leaf a | Node (Tree a) (Tree a)

type family F (a :: Type) :: Type where
  F [a] = Tree a
  F (a, a) = Bool

g :: a -> F a
g = Leaf


