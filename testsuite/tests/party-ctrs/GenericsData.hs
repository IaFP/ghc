{-# LANGUAGE QuantifiedConstraints, DataKinds, PolyKinds, RankNTypes, TypeFamilies
    , TypeOperators, UndecidableInstances, ExplicitNamespaces, GeneralisedNewtypeDeriving #-}

module GenericsData where
import GHC.Types (type (@), Total)
import GHC.Generics(Generic, Generic1)
import GHC.Base


data f @ p => M1 (i :: Type) (f :: k -> Type) (p :: k) =
    M1 { unM1 :: f p }
  deriving (Eq, Functor
           -- , Generic waiting on alex's change
           , Generic1
           )

deriving instance (Read (f p)) => Read (M1 i f p)
-- deriving instance (Functor f) => Functor (M1 i f)

-- data Ord a => T a = L | B a (T a) (T a)
--   deriving (Eq, Show)

-- deriving instance Functor T
-- deriving instance Generic1 T
-- deriving instance Generic (T a) -- waiting on WF_TyFam fix to be in
-- data Ord a => Set a = Set (T a)
-- deriving instance Show a => Show (Set a)
-- deriving instance Functor (Set)

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
