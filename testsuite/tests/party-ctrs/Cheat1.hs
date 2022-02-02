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
module Test where

import GHC.Exts

-- type family (@@) (f :: k1 -> k) :: k1 -> Constraint
type family (@) (f :: k1 -> k) (t :: k1) :: Constraint

data Ord a => Sample a = SOrd a | SEq a

data BST a = Leaf | Branch a (BST a) (BST a)
type instance BST @ a = Ord a

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

class f @ a => Cheat f a
instance f @ a => Cheat f a

type Total f = forall a. Cheat f a

f :: (Total f, Functor' f) => (a -> b) -> f a -> f b
f = fmap'

-- [1 of 1] Compiling Test             ( Test.hs, interpreted )
--
-- Test.hs:54:5: error:
--     * Could not deduce: (f @@ a, f @@ b) arising from a use of fmap'
--       from the context: (Total f, Functor' f)
--         bound by the type signature for:
--                    f :: forall (f :: * -> *) a b.
--                         (Total f, Functor' f) =>
--                         (a -> b) -> f a -> f b
--         at Test.hs:53:1-52
--     * In the expression: fmap'
--       In an equation for `f': f = fmap'
--     * Relevant bindings include
--         f :: (a -> b) -> f a -> f b (bound at Test.hs:54:1)
--    |
-- 54 | f = fmap'
--    |     ^^^^^
