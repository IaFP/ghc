{-# LANGUAGE NoImplicitPrelude #-}

module ShouldFail where

import FakePrelude

data {- Ord a => -} BST a = Leaf | Node a (BST a) (BST a)

-- This is incorrect and should fail as BST (a -> b) cannot be ordered.
-- The reason is that |/- Ord (a -> b)
instance Functor BST where
  fmap f Leaf = Leaf
  fmap f (Node d lt rt) = Node (f d) (fmap f lt) (fmap f rt)

