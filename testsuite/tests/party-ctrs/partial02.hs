{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -itestsuite/tests/party-ctrs #-}

module ShouldCompile where

import FakePrelude

data Tree a = Leaf a
            | Node a (Tree a) (Tree a)

-- This is fine, as we do not have any implicit assumtions as to
-- what should be allowed in a Tree structure (compare it with BST)
instance Functor Tree where
  fmap f (Leaf d) = Leaf (f d)
  fmap f (Node d lt rt) = Node (f d) (fmap f lt) (fmap f rt)

