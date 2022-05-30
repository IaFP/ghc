{-# LANGUAGE CPP #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGAUGE ExplicitNamespaces #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances, QuantifiedConstraints #-}

module PartialDataTypes where
import GHC.Types (WDT, type (@), Total)

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

emptyDT :: Ord a => DT a -- need to add Ord a here as emptyBST needs it.
emptyDT = MkDT { bstFld = emptyBST }

destDT (MkDT b) = b

data ParDataTy1 m a = MkDT1 {unDT1 :: m a}

-- destParDataTy1 :: ParDataTy1 m a -> m a
destParDataTy1 (MkDT1 f) =  f

pdt1 :: k -> ParDataTy1 Id k
pdt1 k = MkDT1 (Id k)

mapPartDataTy1 :: (Functor m, m @ a, m @ b) => (a -> b) -> ParDataTy1 m a -> ParDataTy1 m b
mapPartDataTy1 f h = MkDT1 $ fmap f (destParDataTy1 h)

instance (Functor m, Total m) => Functor (ParDataTy1 m) where
  fmap = mapPartDataTy1



data f ~ Maybe => WeirdDTy f a = MkWeirdTy {unWDTy :: f a}
destWDty (MkWeirdTy f) = f
