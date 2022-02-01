{-# LANGUAGE PartialTypeConstructors #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Redirection where

type family CF a where
    CF [a] = Bool

g :: a -> CF (Maybe a)
g = undefined

f :: a -> CF (Maybe a)
f = g
