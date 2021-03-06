{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Const
-- Copyright   :  Conor McBride and Ross Paterson 2005
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable

-- The 'Const' functor.
--
-- @since 4.9.0.0

module Const where

import Data.Bits (Bits, FiniteBits)
import Data.Foldable (Foldable(foldMap))
import Foreign.Storable (Storable)
import GHC.Base (Applicative (..))
import GHC.Ix (Ix)
import GHC.Enum (Bounded, Enum)
import GHC.Float (Floating, RealFloat)
import GHC.Generics (Generic, Generic1)
import GHC.Num (Num)
import GHC.Real (Fractional, Integral, Real, RealFrac)
import GHC.Read (Read(readsPrec), readParen, lex)
import GHC.Show (Show(showsPrec), showParen, showString)

-- | The 'Const' functor.
newtype Const a b = Const { getConst :: a }
    deriving ( Bits       -- ^ @since 4.9.0.0
             , Bounded    -- ^ @since 4.9.0.0
             , Enum       -- ^ @since 4.9.0.0
             , Eq         -- ^ @since 4.9.0.0
             , FiniteBits -- ^ @since 4.9.0.0
             , Floating   -- ^ @since 4.9.0.0
             , Fractional -- ^ @since 4.9.0.0
             , Generic    -- ^ @since 4.9.0.0
             , Generic1   -- ^ @since 4.9.0.0
             , Integral   -- ^ @since 4.9.0.0
             , Ix         -- ^ @since 4.9.0.0
             , Semigroup  -- ^ @since 4.9.0.0
             , Monoid     -- ^ @since 4.9.0.0
             , Num        -- ^ @since 4.9.0.0
             , Ord        -- ^ @since 4.9.0.0
             , Real       -- ^ @since 4.9.0.0
             , RealFrac   -- ^ @since 4.9.0.0
             , RealFloat  -- ^ @since 4.9.0.0
             , Storable   -- ^ @since 4.9.0.0
             )

-- | This instance would be equivalent to the derived instances of the
-- 'Const' newtype if the 'getConst' field were removed
--
-- @since 4.8.0.0
instance Read a => Read (Const a b) where
    readsPrec d = readParen (d > 10)
        $ \r -> [(Const x,t) | ("Const", s) <- lex r, (x, t) <- readsPrec 11 s]

-- | This instance would be equivalent to the derived instances of the
-- 'Const' newtype if the 'getConst' field were removed
--
-- @since 4.8.0.0
instance Show a => Show (Const a b) where
    showsPrec d (Const x) = showParen (d > 10) $
                            showString "Const " . showsPrec 11 x

-- | @since 4.7.0.0
instance Foldable (Const m) where
    foldMap _ _ = mempty

-- | @since 2.01
instance Functor (Const m) where
    fmap _ (Const v) = Const v

-- | @since 2.0.1
instance Monoid m => Applicative (Const m) where
    pure _ = Const mempty
    liftA2 _ (Const x) (Const y) = Const (x `mappend` y)
    -- (<*>) = coerce (mappend :: m -> m -> m)
    (Const a) <*> (Const b) = Const (a `mappend` b)
-- This is pretty much the same as
-- Const f <*> Const v = Const (f `mappend` v)
-- but guarantees that mappend for Const a b will have the same arity
-- as the one for a; it won't create a closure to raise the arity
-- to 2.
