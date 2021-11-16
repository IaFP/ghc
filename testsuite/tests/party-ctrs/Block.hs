{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Block where

import GHC.Types ({-type (@@), Total,-} Type)
-- -----------------------------------------------------------------------------
-- Shapes: Open and Closed

data Tree a = Leaf | Node a (Tree a) (Tree a)


#define _UPK_(x) {-# UNPACK #-} !(x)

data TChan a = TChan _UPK_(TVar (TVar (TList a)))
                     _UPK_(TVar (TVar (TList a)))
  -- deriving Eq

type TVarList a = TVar (TList a)

data TList a = TNil | TCons a _UPK_(TVar (TList a))

data TVar a = TVar a

data Rep (f :: Type -> Type)  (a :: Type) = MkRep {mkRep :: f a}

-- | Used at the type level to indicate "open" vs "closed" structure.
data Ext
  = O
  | C

data TreeE (ex :: Ext) a where
  LeafE :: a -> TreeE 'C a
  NodeE :: a -> TreeE 'O a -> TreeE 'O a -> TreeE 'O a


mapTreeE :: forall ex ex' a b. (a -> b) -> TreeE ex a -> TreeE ex b
mapTreeE f (LeafE a) = LeafE (f a)
mapTreeE f (NodeE a lt rt) = NodeE (f a) (mapTreeE f lt) (mapTreeE f rt)




-- should n @@ O be generated?
data Block (n :: Ext -> Type) (e :: Ext) where
  MkBlock  :: Block n O -> n O -> Block n O
  -- BlockCO  :: {-(Total n, Total (n C))              => -} n C O       -> Block n O O -> Block n C O
  -- BlockCC  :: {-(Total n, Total (n O), Total (n C)) =>  -} n C O       -> Block n O O -> n O C        -> Block n C C  
{-
  BNil    :: Block n O O
  BMiddle :: n O O                      -> Block n O O
  BCat    :: Block n O O -> Block n O O -> Block n O O
  BSnoc   :: Block n O O -> n O O       -> Block n O O
  BCons   :: n O O       -> Block n O O -> Block n O O
-}


-- -----------------------------------------------------------------------------
-- Mapping
{-
-- | map a function over the nodes of a 'Block'
mapBlock :: (forall e x. Total n => n e x -> n' e x) -> Block n e x -> Block n' e x
mapBlock f (BlockCO n b  ) = BlockCO (f n) (mapBlock f b)
mapBlock f (BlockOC   b n) = BlockOC       (mapBlock f b) (f n)
mapBlock f (BlockCC n b m) = BlockCC (f n) (mapBlock f b) (f m)
-- mapBlock _  BNil           = BNil
-- mapBlock f (BMiddle n)     = BMiddle (f n)
-- mapBlock f (BCat b1 b2)    = BCat    (mapBlock f b1) (mapBlock f b2)
-- mapBlock f (BSnoc b n)     = BSnoc   (mapBlock f b)  (f n)
-- mapBlock f (BCons n b)     = BCons   (f n)  (mapBlock f b)

-- | A strict 'mapBlock'
mapBlock' :: (forall e x. n e x -> n' e x) -> (Block n e x -> Block n' e x)
mapBlock' f = mapBlock3' (f, f, f)
-}
-- | map over a block, with different functions to apply to first nodes,
-- middle nodes and last nodes respectively.  The map is strict.
--
{-
mapBlock3' :: forall n n' e x .
              (Total n, Total n')
           => ( n C O -> n' C O
              , n O O -> n' O O
              , n O C -> n' O C)
           -> Block n e x -> Block n' e x
mapBlock3' (f, m, l) b = go b
-}
-- go :: Block n e x -> Block n' e x
-- go (BlockOC b y)   = BlockOC b y
