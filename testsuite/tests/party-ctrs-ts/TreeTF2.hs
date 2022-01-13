{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeConstructors #-}

module TreeTF1 where

import GHC.Types (Type)

data Ord a => Tree a = Leaf a | Node (Tree a) (Tree a)

type family F (a :: Type) :: Type
type instance F [a] = Tree a

-- The work done in TreeTF1.hs should happen
-- at typechecking now, leaving g well typed.
g :: a -> F [a]
g = Leaf

