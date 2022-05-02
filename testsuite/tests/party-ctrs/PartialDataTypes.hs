{-# LANGUAGE CPP #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGAUGE ExplicitNamespaces #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances, QuantifiedConstraints #-}

module PartialDataTypes where
import GHC.Types (WFT, type (@))

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

  
-- data ParDataTy2 m a = MkDT2 (a -> m a)

-- data ParDataTy3 m a = MkDT3 (a -> m a)
--                     | MkDT3' a

-- @since 4.10.0.0
-- data SomeNat    = forall n. (C n) => SomeNat    (Proxy n)


pdt1 :: k -> ParDataTy1 Id k
pdt1 k = MkDT1 (Id k)


-- data Ord a => Ordered a = MkOrdered a
-- -- MkOrdered :: Ordered
-- -- MkOrdered :: Ord a => Ordered a

-- o1 :: Ordered Int
-- o1 = MkOrdered 1

-- bst :: BST (Ordered Int)
-- bst = Node (MkOrdered 1) Leaf Leaf

-- pdata1 :: ParDataTy1 Only Int
-- pdata1 = MkDT1 (Only 3)
