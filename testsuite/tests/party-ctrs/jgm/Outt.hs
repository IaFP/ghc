{-# LANGUAGE TypeFamilies, PartialTypeConstructors #-}
module Outt (F, foo) where

type family F (a :: *) :: *

foo :: a -> F a -> F a
foo x y = y
