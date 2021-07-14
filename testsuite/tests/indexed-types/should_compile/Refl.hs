{-# LANGUAGE TypeFamilies #-}

module Refl where

import Data.Kind (Type)
import GHC.Types (type (@@))

type family T a :: Type -> Type

foo :: a x -> a y
foo = undefined

bar :: (T a @@ x, T a @@ y) => a -> T a x -> T a y
bar x t = foo t

{- GHC complains that it could not deduce (T a x ~ T a x) where problem is
that with -dppr-debug, we get "x{tv a7z} [sk]" on the lhs and "x{tv a7C}
[box]" on the rhs
 -}

