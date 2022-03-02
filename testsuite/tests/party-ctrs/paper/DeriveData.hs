{-# LANGUAGE QuantifiedConstraints, DataKinds, PolyKinds, RankNTypes, TypeFamilies
    , TypeOperators, UndecidableInstances, ExplicitNamespaces, GeneralisedNewtypeDeriving, DerivingStrategies, DeriveAnyClass #-}

module DeriveData where

import GHC.Types
import GHC.Base
import GHC.Ix
import GHC.Enum (Bounded, Enum)
import GHC.Float (Floating, RealFloat)
import GHC.Num (Num)
import GHC.Real (Fractional, Integral, Real, RealFrac)
import Data.Bits (Bits, FiniteBits)
import Foreign.Storable (Storable)

data f @ p => M1 (i :: Type) (f :: k -> Type) (p :: k) =
    M1 { unM1 :: f p }
    deriving (Eq           
           , Ord           
           , Read          
           , Show          
           , Functor       
           {- , Applicative
           , Alternative
           , Monad
           , MonadPlus-}
           , Monoid
           , Semigroup
           )

data Const a b =  Const { unConst :: a }
  deriving (Eq           
           , Ord           
           , Read          
           , Show          
           , Functor       
           , Applicative
           , Alternative
           , Monad
           , MonadPlus
           , Monoid
           , Semigroup
           )


data Ord a => T a = L | B a (T a) (T a)
  deriving (Eq, Ord, Show, Functor, Monoid, Semigroup {- Applicative, , Alternative, Monad, MonadPlus-} )

