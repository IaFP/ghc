{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module ES where

import GHC.Types (Type, Total, WDT)

class HasF a where
  type family F a
  foo :: a -> F a

data Monad m
  => Foobar (m :: Type -> Type) = forall a. HasF a => Foobar (a -> m (F a))


-- Challenge:
-- Make the following type!
--
-- Have tried:
--  * Add WDT (F a) on both 
--  * Add (F a ~ F a)
--  * Constraining the data type with above.
--  * Adding WDT (F a) => HasF a

noDeduce :: Total m => Foobar m
noDeduce = Foobar (\a -> return (foo a))

