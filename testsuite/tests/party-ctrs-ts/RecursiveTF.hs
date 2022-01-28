{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeConstructors #-}

module RecursiveTF where

data Ord a => Tree a = Leaf a | Node (Tree a) (Tree a)

-- Does WF_F resolve constraints correctly in the recursive case?
type family F (a :: *) :: *
type instance F (a, a) = F [a]
type instance F [a] = Tree a

g :: a -> F (a, a)
g = Leaf
