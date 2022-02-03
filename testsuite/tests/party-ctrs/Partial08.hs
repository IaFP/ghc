{-# LANGUAGE KindSignatures, PolyKinds, TypeOperators, TypeFamilies #-}
{-# LANGUAGE RankNTypes, FlexibleContexts , ConstrainedClassMethods #-}
{-# LANGUAGE PartialTypeConstructors, QuantifiedConstraints, DatatypeContexts #-}
{-# LANGUAGE DeriveAnyClass, MagicHash, UnboxedTuples, DataKinds, TypeApplications, UnliftedNewtypes #-}
{-# LANGUAGE DerivingStrategies, MultiParamTypeClasses, DeriveGeneric, MagicHash #-}
{-# LANGUAGE PartialTypeConstructors #-}

module ConstraintFamilies where


newtype K a b = K {unK :: a}

instance Functor (K a) where
  fmap f (K x) = K x

instance Monoid a => Applicative (K a) where
  pure _ = K mempty
  f <*> x = K (unK f `mappend` unK x)

newtype Id a = Id {unId :: a}

instance Functor Id where
    fmap f x = Id (f (unId x))

instance Applicative Id where
     pure      = Id
     f <*> x   = Id (unId f (unId x))

newtype C f g a = Comp {unComp :: f (g a) }

instance (Functor f, Functor g) => Functor (C f g) where
    fmap f (Comp fgx) = Comp (fmap (fmap f) fgx)

instance (Applicative f, Applicative g) => Applicative (C f g) where
    pure      = Comp . pure . pure
    f <*> x  = Comp (pure (<*>) <*> unComp f <*> unComp x)
