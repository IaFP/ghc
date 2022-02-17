{-# LANGUAGE RankNTypes, FlexibleInstances
             , TypeSynonymInstances
             , FlexibleContexts, ExistentialQuantification
             , ScopedTypeVariables, GeneralizedNewtypeDeriving
             , StandaloneDeriving
             , MultiParamTypeClasses
             , UndecidableInstances
             , ScopedTypeVariables, CPP, DeriveDataTypeable
             , PatternGuards
  #-}
{-# LANGUAGE PartialTypeConstructors, QuantifiedConstraints, DatatypeContexts
           , DefaultSignatures, ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass, DataKinds, TypeApplications, UnliftedNewtypes, TypeFamilies, TypeOperators, PolyKinds #-}

module SkolemTest where

import GHC.Types (type (@), Total)

data Hot m = forall n. Hot (forall a. m a -> n a) (forall b. n b -> m b)

class C1 (b:: k)

class C2 (b :: k -> k')


ty1 :: forall m a. (Monad m) => m a -> m a
ty1 = \x -> x

ty2 :: forall m n a. (Monad m) => (forall b. m b -> n b) -> m a -> n a
ty2 f a = f a


test :: Monad m => Hot m -> m a -> m a
test h a = case h of
             Hot f g -> (g . f) a

newtype Q q x = Q {unQ :: x -> q}

blah :: (C2 t, C1 d) => (d -> q)
               -> (forall e. C1 e => t e -> q)
               -> d -> q
blah def ext = unQ 
