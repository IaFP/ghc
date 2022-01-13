{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeConstructors #-}

module TreeTF1 where

data Ord a => Tree a = Leaf a | Node (Tree a) (Tree a)

type family F (a :: *) :: *
type instance F [a] = Tree a

-- Should compile (doesn't currently)
g :: a -> F [a]
g = Leaf
