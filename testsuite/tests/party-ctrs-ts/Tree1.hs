{-# LANGUAGE PartialTypeConstructors #-}

module Tree1 where

data Ord a => Tree a = Leaf a | Node (Tree a) (Tree a)

-- Should compile
g :: a -> Tree a
g = Leaf


