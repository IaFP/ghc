{-# LANGUAGE InstanceSigs, KindSignatures, GADTs, RankNTypes, ConstraintKinds, ScopedTypeVariables, FlexibleInstances, ExplicitNamespaces, TypeOperators #-}

module SubTy where

import GHC.Exts (Constraint)

import Control.Applicative
import Control.Monad
import GHC.Types (type (@))
-------------------------------------------------------------------------------------------------

data NF :: (* -> Constraint) -> (* -> *) -> * -> * where
  FMap :: (c x, t @ x) => (x -> a) -> t x -> NF c t a

instance Functor (NF c t) where
  fmap :: (a -> b) -> NF c t a -> NF c t b
  fmap g (FMap h tx)  = FMap (g . h) tx  -- composition law

liftNF :: c a => t a -> NF c t a
liftNF ta = FMap id ta    -- identity law

foldNF :: (forall x. c x => (x -> a) -> t x -> r) -> NF c t a -> r
foldNF fmp (FMap g tx) = fmp g tx

lowerNF :: (forall x. c x => (x -> a) -> t x -> t a) -> NF c t a -> t a
lowerNF  = foldNF
