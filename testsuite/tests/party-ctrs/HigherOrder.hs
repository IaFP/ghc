{-# LANGUAGE CPP #-}
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
{-# LANGUAGE  QuantifiedConstraints, DatatypeContexts
           , DefaultSignatures, ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass, DataKinds, TypeApplications, UnliftedNewtypes, TypeFamilies, TypeOperators, PolyKinds #-}

module HigherOrder where
import GHC.Types

class CM (m :: Type -> Type)


data HoT = HoT { hte :: forall m. HoTExis m
               , init :: forall m a. n Int -> m a }

data HoTExis m = forall (n:: Type -> Type). HoTExis (forall a. n a -> m a) (forall a. m a -> n a)

-- f :: CM m => HoT -> m a
outer :: forall m a. (Monad m, CM m) => HoT -> a -> m a
outer HoT{hte=e} a = case e of
             HoTExis e l -> e helper


helper :: forall m n a. (CM m, CM n)
       => (forall b. m b -> n b) -> HoT -> n Int -> n a
helper l h init = 
