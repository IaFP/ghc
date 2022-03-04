{-# LANGUAGE QuantifiedConstraints, DataKinds, PolyKinds, RankNTypes, TypeFamilies
    , TypeOperators, UndecidableInstances, ExplicitNamespaces, GeneralisedNewtypeDeriving #-}

module GenericsData where
import GHC.Types (type (@), Total)
import GHC.Generics(Generic, Generic1)
import GHC.Base


data f @ p => M1 (i :: Type) (f :: k -> Type) (p :: k) =
    M1 { unM1 :: f p }
  deriving (Eq
           , Functor
           , Generic
           , Generic1
           )
