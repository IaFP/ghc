{-# LANGUAGE PartialTypeConstructors #-}
{-# LANGUAGE FlexibleContexts #-}

module Tree2 where

data Ord a => Tree a = Leaf a | Node (Tree a) (Tree a)

-- Should compile
g :: a -> Tree a
g = Leaf

-- Should fail
f = g id
