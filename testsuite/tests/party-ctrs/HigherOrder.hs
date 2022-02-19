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
import Control.Monad.Trans

class MonadIO m => CM (m :: Type -> Type)


data HoT = HoT { hte :: forall m. CM m => HoTExis m
               , wge :: forall n a. (n Int -> n a) -> n a }

data HoTExis m = forall n. (CM n) =>  HoTExis (forall a. n a -> m a)
                                              (forall a. m a -> n a)

-- f :: CM m => HoT -> m a
outer :: forall m a. (Monad m, CM m) => HoT -> a -> m a
outer ht@HoT{hte=hte@(HoTExis e l), wge=wge} a = e $ wge (helper l ht)


helper :: forall m n a. (CM m, CM n)
       => (forall b. m b -> n b) -> HoT -> n Int -> n a
helper l h init = undefined
