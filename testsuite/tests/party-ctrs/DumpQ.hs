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

module DumpQ where

import Data.Data
import Data.Typeable
import GHC.Types (type (@), Total, Total2)

newtype Q q x = Q { unQ :: x -> q }

-- | Type extension of queries for type constructors
ext1Q :: (
  Total t, 
  Data d, Typeable t)
      => (d -> q)
      -> (forall e. Data e => t e -> q)
      -> d -> q
ext1Q def ext = unQ (maybe (Q def) id $ dataCast1 (Q ext))

-- | Flexible type extension
ext1 :: (
  Total c, Total t,
  Data a, Typeable t)
     => c a
     -> (forall d. Data d => c (t d))
     -> c a
ext1 def ext = maybe def id (dataCast1 ext)
