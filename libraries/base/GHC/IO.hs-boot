{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PartialTypeConstructors, TypeOperators, TypeFamilies #-}
{-# OPTIONS -fno-enable-rewrite-rules #-}

module GHC.IO where

import GHC.Types
import GHC.Integer () -- See Note [Depend on GHC.Integer] in GHC.Base
import {-# SOURCE #-} GHC.Exception.Type (SomeException)

mplusIO :: IO a -> IO a -> IO a
mkUserError :: [Char] -> SomeException
