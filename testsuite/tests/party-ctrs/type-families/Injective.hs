{-# LANGUAGE TypeFamilies, TypeFamilyDependencies, PartialTypeConstructors #-}
module Injective where

type family Id a = r | r -> a
type instance Id a = a

-- This should compile...
id :: Id t -> Id t
id x = x
