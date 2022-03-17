{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeConstructors #-}

module ClosedTF2 where

type family F (a :: *) :: * where
  F (a, a) = F a
--  F [a] = Tree a

-- data Ord a => Tree a = Leaf a | Node (Tree a) (Tree a)

-- g :: a -> F (a, a)
-- g = Leaf
