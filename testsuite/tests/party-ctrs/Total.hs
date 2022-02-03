{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeConstructors, TypeOperators, UndecidableInstances, StandaloneDeriving #-}

module Total where

import GHC.Types (type (@@), Total)

data Tree a where
  Leaf :: Tree a
  Node :: a -> Tree a -> Tree a -> Tree a

instance Total Tree

deriving instance (Show a) => Show (Tree a)

fmap'tree :: (a -> b) -> Tree a -> Tree b
fmap'tree f Leaf = Leaf
fmap'tree f (Node d lt rt) = Node (fmap f d) (fmap f lt) (fmap f rt)

instance Functor BST where
   fmap = fmap'tree

