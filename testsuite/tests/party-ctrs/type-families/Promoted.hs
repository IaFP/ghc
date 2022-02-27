module Problem10 where

data Nat = Z | S Nat

data Tree a = Leaf a | Node (Tree a) (Tree a)
