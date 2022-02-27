{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeConstructors #-}

module RecursiveTFOpen where

data Ord a => Tree a = Leaf a | Node (Tree a) (Tree a)

-- Does WF_F resolve constraints correctly in the recursive case?
type family F (a :: *) :: *

-- We should see axioms for
--  WF_F (a, a) ~ WF_F [a]
--  WF_F [a]    ~ Ord a
type instance  F (a, a) = F [a]
type instance  F [a] = Tree a

-- This should resolve with F (a, a) ~ Tree a,
-- so g will have constraint Ord a =>.
g :: a -> F (a, a)
g = Leaf

-- This should have WF_F constraint, as (F a) does not normalize.
h :: F a -> a
h = undefined
