{-# LANGUAGE CPP #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGAUGE ExplicitNamespaces #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances, QuantifiedConstraints #-}

module PartialDataTypes where
import GHC.Types (WFT, type (@), Total)

data Ord a => BST a = Leaf | Node a (BST a) (BST a)
data Only a = Only a
class C a
data Proxy a
data Id a = Id a

emptyBST :: BST a
emptyBST = Leaf

sbst :: BST Int
sbst = Node 1 Leaf Leaf

-- type family TF a

data DT a = MkDT { bstFld :: BST a }
-- we expect MkNT :: forall a. BST @ a => BST a -> NT a

emptyDT :: Ord a => DT a
emptyDT = MkDT undefined


data ParDataTy1 m a = MkDT1 (m a)

pdt1 :: k -> ParDataTy1 Id k
pdt1 k = MkDT1 (Id k)

mapPartDataTy1 :: (Functor m, m @ a, m @ b) => (a -> b) -> ParDataTy1 m a -> ParDataTy1 m b
mapPartDataTy1 f (MkDT1 h) = MkDT1 (fmap f h)

instance (Functor m, Total m) => Functor (ParDataTy1 m) where
  fmap = mapPartDataTy1
