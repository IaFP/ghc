{-# LANGUAGE PartialTypeConstructors, DefaultSignatures, ExplicitNamespaces, QuantifiedConstraints #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedFFITypes #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TypeFamilyDependencies #-}
#else
{-# LANGUAGE TypeFamilies #-}
#endif
{-# OPTIONS_HADDOCK hide, not-home #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


module RepShenigans where

import GHC.Types (Total, type(@), Type)
import Control.Monad.Cont (ContT, runContT)
import GHC.Generics

class (Total m, Monad m) => StatefulGen g m where
  pfft :: g -> m a

class GUniformRange f where
  guniformRM :: StatefulGen g m => (f a, f a) -> g -> ContT r m (f a)
  gisInRange :: (f a, f a) -> f a -> Bool

class GFinite f where
  -- gcardinality :: Proxy# f -> Cardinality
  toGFinite :: Integer -> f a
  fromGFinite :: f a -> Integer

class Shenanigans a where
  wtf :: (a, a) -> g -> m a

  default wtf :: (
    forall x. Rep a @ x,
    Generic a,
    StatefulGen g m,
    GUniformRange (Rep a)) => (a, a) -> g -> m a
  wtf (a, b) = fmap to . (`runContT` pure) . guniformRM (from a, from b)
  {-# INLINE wtf #-}

finiteUniformM :: forall g m f a. (StatefulGen g m, GFinite f) => g -> m (f a)
finiteUniformM = undefined
{-# INLINE finiteUniformM #-}

uniformViaFiniteM :: (
  forall x. Rep a @ x,
  StatefulGen g m,
  Generic a,
  GFinite (Rep a)) => g -> m a
uniformViaFiniteM = fmap to . finiteUniformM
{-# INLINE uniformViaFiniteM  #-}
