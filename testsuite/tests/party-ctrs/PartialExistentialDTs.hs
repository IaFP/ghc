{-# LANGUAGE CPP #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGAUGE ExplicitNamespaces #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances, QuantifiedConstraints #-}

module PartialExistentialDTs where
import GHC.Types (WFT, type (@))

class C a
data Proxy a

data SomeNat = forall n. C n => SomeNat (Proxy n)

snatpty :: SomeNat -> SomeNat 
snatpty (SomeNat p) = SomeNat p

data Handler m a = forall e . C e => MkHandler (e -> m a)

mapHandler :: (Functor m, m @ a, m @ b) => (a -> b) -> Handler m a -> Handler m b
mapHandler f (MkHandler h) = MkHandler (\e -> fmap f (h e))

data Ord a => Blah a = Blah a
  deriving Functor

data Ord a => OrdList a where
  ONil :: OrdList a 
  OCons :: Blah a -> OrdList a -> OrdList a

-- assume f is monotonus
mapOrdList :: (a -> b) -> OrdList a -> OrdList b
mapOrdList _ ONil = ONil
mapOrdList f (OCons b l) = OCons (fmap f b) (mapOrdList f l)

instance Functor OrdList where
  fmap = mapOrdList
