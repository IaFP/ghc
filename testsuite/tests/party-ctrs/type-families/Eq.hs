{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeConstructors #-}

module Eq where

{--------------------------------------------------------------------------------

This code is causing non-termination atm in Base/Data/Type/Equality.hs.

--------------------------------------------------------------------------------}

type (==) :: k -> k -> Bool
type family a == b where
  f a == g b = f == g && a == b
  a == a = 'True
  _ == _ = 'False
