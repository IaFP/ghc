{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeConstructors #-}

module ClosedTF2 where

type family F (a :: *) :: * where
  F (a, a) = F a
  F a = Tree a

data Ord a => Tree a = Leaf a | Node (Tree a) (Tree a)

--------------------------------------------------------------------------------
-- No normalization
--------------------------------------------------------------------------------

g  :: a -> F a
g = undefined

--------------------------------------------------------------------------------
-- One step of reduction, e.g,
--   F Int ~ Tree Int
--------------------------------------------------------------------------------

g' :: Int -> F Int
g' = Leaf

--------------------------------------------------------------------------------
-- Two steps of reduction, e.g,
-- F (Int, Int) ~ F Int ~ Tree Int
--------------------------------------------------------------------------------
g'' :: Int -> F (Int, Int)
g'' = Leaf
