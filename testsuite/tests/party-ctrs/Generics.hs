{-# LANGUAGE DataKinds, PolyKinds, RankNTypes, TypeFamilies, TypeOperators #-}

module Generics where


newtype Down a = Down
    { getDown :: a -- ^ @since 4.14.0.0
    }
    deriving
      ( Eq        -- ^ @since 4.6.0.0
      , Num       -- ^ @since 4.11.0.0
      , Semigroup -- ^ @since 4.11.0.0
      , Monoid    -- ^ @since 4.11.0.0
      , Bits       -- ^ @since 4.14.0.0
      , Bounded    -- ^ @since 4.14.0.0
      , Enum       -- ^ @since 4.14.0.0
      , FiniteBits -- ^ @since 4.14.0.0
      , Floating   -- ^ @since 4.14.0.0
      , Fractional -- ^ @since 4.14.0.0
      , Integral   -- ^ @since 4.14.0.0
      , Ix         -- ^ @since 4.14.0.0
      , Real       -- ^ @since 4.14.0.0
      , RealFrac   -- ^ @since 4.14.0.0
      , RealFloat  -- ^ @since 4.14.0.0
      , Storable   -- ^ @since 4.14.0.0
      )
