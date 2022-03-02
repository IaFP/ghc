{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoPartialTypeConstructors #-}

module GHC.Exception.Type
  ( SomeException
  , divZeroException
  , overflowException
  , ratioZeroDenomException
  , underflowException
  ) where

import GHC.Num.Integer ()   -- See Note [Depend on GHC.Num.Integer] in GHC.Base

data SomeException
divZeroException, overflowException,
  ratioZeroDenomException, underflowException :: SomeException
