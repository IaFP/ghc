{-# LANGUAGE QuantifiedConstraints, DataKinds, PolyKinds, RankNTypes, TypeFamilies
    , TypeOperators, UndecidableInstances, ExplicitNamespaces, GeneralisedNewtypeDeriving, DerivingStrategies #-}

module DeriveNewtypes where
import GHC.Types
import GHC.Base
import GHC.Ix
import GHC.Enum (Bounded, Enum)
import GHC.Float (Floating, RealFloat)
import GHC.Num (Num)
import GHC.Real (Fractional, Integral, Real, RealFrac)
import Data.Bits (Bits, FiniteBits)
import Foreign.Storable (Storable)

import DeriveData (T)  

-- Interesting case
newtype f @ p => M1 (i :: Type) (f :: k -> Type) (p :: k) =
    M1 { unM1 :: f p }
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

-- Boring case
newtype Const a b =  Const { unConst :: a }
  deriving (Bits       
           , Bounded   
           , Enum      
           , Eq        
           , FiniteBits
           , Floating  
           , Fractional
           , Integral  
           , Ix        
           , Semigroup 
           , Monoid    
           , Num       
           , Ord       
           , Real      
           , RealFrac  
           , RealFloat 
           , Storable  
           )


newtype Ord a => Set a = Set { unSet ::  T a }
              deriving (Eq, Ord, Show, Functor, Monoid, Semigroup)

