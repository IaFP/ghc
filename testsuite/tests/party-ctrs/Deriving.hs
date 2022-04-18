{-# LANGUAGE QuantifiedConstraints, DataKinds, PolyKinds, RankNTypes, TypeFamilies
    , TypeOperators, UndecidableInstances, ExplicitNamespaces  #-}

module Deriving where
-- import GHC.Types (type (@), Total)
import GHC.Generics(Generic, Generic1)
import GHC.Base


data Ord a => T a = L | B a (T a) (T a)
  deriving (Eq
           , Show
           , Functor
           , Generic1
           , Foldable
           , Traversable
           , Generic
           )



