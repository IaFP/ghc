{-# LANGUAGE KindSignatures, PolyKinds, TypeOperators, TypeFamilies, GADTs #-}
{-# LANGUAGE RankNTypes, FlexibleContexts , ConstrainedClassMethods #-}
{-# LANGUAGE QuantifiedConstraints, DatatypeContexts, DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass, MagicHash, UnboxedTuples, DataKinds, TypeApplications, UnliftedNewtypes #-}

module Tree where

import GHC.Types (type (@@))

data Tree a = Leaf a
            | Node a (Tree a) (Tree a)
            deriving (Show)

deriving instance Eq a => Eq (Tree a)

-- This is fine, as we do not have any implicit assumtions as to
-- what should be allowed in a Tree structure (compare it with BST)
instance Functor Tree where
  fmap f (Leaf d) = Leaf (f d)
  fmap f (Node d lt rt) = Node (f d) (fmap f lt) (fmap f rt)

