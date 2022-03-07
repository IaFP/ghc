{-# LANGUAGE QuantifiedConstraints, DataKinds, PolyKinds, RankNTypes, TypeFamilies
    , TypeOperators, UndecidableInstances, ExplicitNamespaces, GeneralisedNewtypeDeriving #-}

module GenericsNT where
import GHC.Types (type (@), Total)
import GHC.Generics(Generic, Generic1)
import GHC.Base


newtype f @ p => M1 (i :: Type) (f :: k -> Type) (p :: k) =
    M1 { unM1 :: f p }


deriving instance (Eq (f p)) => Eq (M1 i f p) -- this fails because of an extra constraint in the rhs of the typeid of == @M1
deriving instance (Read (f p)) => Read (M1 i f p)
deriving instance (Functor (f p)) => Functor (M1 i f)
    
  -- deriving ( Eq       -- ^ @since 4.7.0.0
  --          , Ord      -- ^ @since 4.7.0.0
  --          , Read     -- ^ @since 4.7.0.0
  --          , Show     -- ^ @since 4.7.0.0
  --          , Functor  -- ^ @since 4.9.0.0
  --          -- , Generic  -- ^ @since 4.7.0.0
  --          -- , Generic1 -- ^ @since 4.9.0.0
  --          )

-- deriving instance (Total f, Applicative f) => Applicative (M1 i f)

-- -- | @since 4.9.0.0
-- deriving instance (Total f, Alternative f) => Alternative (M1 i f)

-- -- | @since 4.9.0.0
-- deriving instance (Total f, Monad f) => Monad (M1 i f)

-- -- | @since 4.9.0.0
-- deriving instance (Total f, MonadPlus f) => MonadPlus (M1 i f)

-- -- | @since 4.12.0.0
-- deriving instance Semigroup (f p) => Semigroup (M1 i f p)

-- -- | @since 4.12.0.0
-- deriving instance Monoid (f p) => Monoid (M1 i f p)

-- -- | Meta-information (constructor names, etc.)

-- -- deriving instance (Total f, Functor f) => Functor (M1 i c f)
-- deriving instance (Total f, Functor f) => Generic (M1 i f p)
-- deriving instance (Total f, Functor f) => Generic1 (M1 i f)
