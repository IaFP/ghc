{-# LANGUAGE TypeFamilies, PartialTypeConstructors #-}
module A.Out (F, foo) where

type family F (a :: *) :: *

foo :: a -> F a -> F a
foo x y = y
