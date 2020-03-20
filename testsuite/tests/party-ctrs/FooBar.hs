{-# LANGUAGE PartialTypeConstructors, TypeOperators #-}

-- The instances below have large demands, though I think they're pretty sane.
-- {-# LANGUAGE UndecidableInstances #-}

-- This test uses recursive dictionaries
-- where we do addSolvedDict before solving sub-goals

module FooBar where

data Foo f a = MkFoo (f a)
-- Foo @@ f  = ()
-- Foo f @@ a = f @@ a

data Bar x a = MkBar (x (Bar x) a)
-- Bar @@ x = x @@ Bar x
-- Bar x @@ a = x (Bar x) @@ a

foobar :: Bar Foo Bool
foobar = undefined
