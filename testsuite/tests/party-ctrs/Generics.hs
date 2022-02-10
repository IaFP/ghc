{-# LANGUAGE DataKinds, PolyKinds, RankNTypes, TypeFamilies, TypeOperators, UndecidableInstances #-}

module Generics where
import GHC.Types

-- newtype Down a = Down
--     { getDown :: a -- ^ @since 4.14.0.0
--     }
--     deriving
--       ( Eq        -- ^ @since 4.6.0.0
--       , Ord
--       , Num       -- ^ @since 4.11.0.0
--       , Semigroup -- ^ @since 4.11.0.0
--       , Monoid    -- ^ @since 4.11.0.0
--       -- , Bits       -- ^ @since 4.14.0.0
--       , Bounded    -- ^ @since 4.14.0.0
--       , Enum       -- ^ @since 4.14.0.0
--       -- , FiniteBits -- ^ @since 4.14.0.0
--       , Floating   -- ^ @since 4.14.0.0
--       , Fractional -- ^ @since 4.14.0.0
--       , Integral   -- ^ @since 4.14.0.0
--       -- , Ix         -- ^ @since 4.14.0.0
--       , Real       -- ^ @since 4.14.0.0
--       , RealFrac   -- ^ @since 4.14.0.0
--       , RealFloat  -- ^ @since 4.14.0.0
--       -- , Storable   -- ^ @since 4.14.0.0
--       )


-- data V1 (p :: k)
--   deriving ( Eq       -- ^ @since 4.9.0.0
--            , Ord      -- ^ @since 4.9.0.0
--            , Read     -- ^ @since 4.9.0.0
--            , Show     -- ^ @since 4.9.0.0
--            , Functor  -- ^ @since 4.9.0.0
--            -- , Generic  -- ^ @since 4.9.0.0
--            -- , Generic1 -- ^ @since 4.9.0.0
--            )

data Rec1 (f :: k -> Type) (p :: k) = Rec1 { unRec1 :: f p }
  deriving ( Eq
           , Ord      -- ^ @since 4.7.0.0
           , Read     -- ^ @since 4.7.0.0
           , Show     -- ^ @since 4.7.0.0
           -- , Functor  -- ^ @since 4.9.0.0
           -- , Generic  -- ^ @since 4.7.0.0
           -- , Generic1 -- ^ @since 4.9.0.0
           )
{-
-- deriving instance (Eq (f p)) => Eq (Rec1 f p)
data (:*:) (f :: k -> Type) (g :: k -> Type) (p :: k) = f p :*: g p
  deriving ( Eq       -- ^ @since 4.7.0.0
           , Ord      -- ^ @since 4.7.0.0
           , Read     -- ^ @since 4.7.0.0
           , Show     -- ^ @since 4.7.0.0
           -- , Functor  -- ^ @since 4.9.0.0
           -- , Generic  -- ^ @since 4.7.0.0
           -- , Generic1 -- ^ @since 4.9.0.0
           )
-- deriving instance (Total f, Total g, Functor f, Functor g) => Functor (f :*: g)
-- deriving instance (Total f, Total g, Functor f, Functor g) => Generic ((f :*: g) p)
-- deriving instance (Total f, Total g, Functor f, Functor g) => Generic1 (f :*: g)
data (:+:) (f :: k -> Type) (g :: k -> Type) (p :: k) = L1 (f p) | R1 (g p)
  deriving ( Eq       -- ^ @since 4.7.0.0
           , Ord      -- ^ @since 4.7.0.0
           , Read     -- ^ @since 4.7.0.0
           , Show     -- ^ @since 4.7.0.0
           )
-}
