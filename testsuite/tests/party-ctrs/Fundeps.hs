{-# Language FunctionalDependencies, UndecidableInstances #-}

module Fundeps where

class C o a b | a -> b
class D a b | a -> b

instance D a b => C Int a b
instance D a b => C Bool a b -- this should fail as they are overlapping

