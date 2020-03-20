{-# LANGUAGE StaticPointers       #-}
{-# LANGUAGE PartialTypeConstructors #-}
module StaticPtr where

import Data.Typeable
import GHC.StaticPtr


class IsStatic p where
    fromStaticPtr :: StaticPtr a -> p a

-- | @since 4.9.0.0
instance StaticPtr.IsStatic StaticPtr where
    fromStaticPtr = id

