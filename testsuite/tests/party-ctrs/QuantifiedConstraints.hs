{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}

module QuantifiedConstraints where

import GHC.Types (Type, type (@@), Constraint)


type family Wf :: (k' -> k) -> k' -> Constraint
-- type instance Wf = forall k' k (a::k' -> k) (b::k'). @@

-- type family Wf (t :: k' -> k) (u :: k') :: Constraint
-- type family Wf2 :: (k'' -> k' -> k) -> k'' -> k' -> k' -> Constraint

-- type Total (t :: k' -> k) = (p ~ Wf t, forall u. p u)
-- type Total2 (t :: k' -> k'' -> k''') = (p ~ WF t, forall u. p u)
  
-- type Sig a = (Total2 (TyFam a))

data Step s a where
  Yield :: a -> s -> Step s a

data Stream m a = forall s. Stream (s -> m (Step s a)) s

newtype Arr to a b = Arr (a `to` b)

-- type family T1 a
-- type family T2 a b

-- type instance T1 Int = T2 Int Int

-- type Total f = (p ~ Wf (Stream m), forall x. p x)
-- This fails though as p is unbound
-- newtype Total f = Total (p ~ Wf f, forall x. p x)

sig :: (p ~ Wf (Stream m), forall x. p x) => a -> Stream m a
sig = undefined

sig' :: (p ~ Wf (Arr to), q ~ Wf (Arr to a), forall x. p x, forall y. q y) => a -> b -> Arr to a b
sig' = undefined
