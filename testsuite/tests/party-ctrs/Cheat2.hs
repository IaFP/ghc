{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Cheat2 where

import GHC.Exts

type family (@) (f :: k1 -> k) :: k1 -> Constraint

data BST a = Leaf | Branch a (BST a) (BST a)
type instance (@) BST = Ord

insert :: BST @ a => a -> BST a -> BST a
insert x Leaf = Branch x Leaf Leaf
insert x t@(Branch y l r)
    | x < y = Branch y (insert x l) r
    | x == y = t
    | otherwise = Branch y l (insert x r)

merge :: BST @ a => BST a -> BST a -> BST a
merge Leaf t = t
merge (Branch y l r) t = insert y (merge l (merge r t))

class Functor' f where
    fmap' :: (f @ a, f @ b) => (a -> b) -> f a -> f b

instance Functor' BST where
    fmap' f Leaf = Leaf
    fmap' f (Branch x l r) = insert (f x) (merge (fmap' f l) (fmap' f r))

-- But now how to handle a partial type constructor with more than one
-- constraint?  I want to be able to define /\ for constraints, but
-- none of the following work.

-- type family (/\) (c :: k -> Constraint) (d :: k -> Constraint) :: k -> Constraint
-- type instance (c /\ d) e = (c e, d e)
-- type (/\) (c :: k -> Constraint) (d :: k -> Constraint) (e :: k) = (c e, d e) -- This works in head (ghc-9.3.20211007) confimed


-- Also, cheaters still never (always) win

class f @ a => Cheat f a -- so does this
instance f @ a => Cheat f a -- and this

type Total f = forall a. Cheat f a

f :: (Total f, Functor' f) => (a -> b) -> f a -> f b
f = fmap'
