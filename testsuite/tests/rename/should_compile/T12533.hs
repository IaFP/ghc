{-# LANGUAGE ScopedTypeVariables, DefaultSignatures #-}
{-# LANGUAGE PartialTypeConstructors #-}

module T12533 where
import GHC.Types (Total)
class Foo x where
  foo :: forall a . Total x => x a -> x a
  default foo :: forall a . Total x => x a -> x a
  foo x = go
    where go :: x a
          go = undefined
