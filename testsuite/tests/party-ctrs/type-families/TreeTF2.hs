c{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeConstructors #-}

module TreeTF2 where

import GHC.Types (Type)

data Ord a => Tree a = Leaf a | Node (Tree a) (Tree a)

type family F (a :: Type) :: Type
type instance F [a] = Tree a

-- F [a] will elaborate to Tree a,
-- which will propagagate Tree @ a ~ Ord a constraints.
g :: a -> F [a]
g = Leaf
