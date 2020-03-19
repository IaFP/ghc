{-# LANGUAGE NoImplicitPrelude #-}
-- This is a small portion of prelude
-- that we want to import in
-- All our test files

module FakePrelude where

data Bool = True | False

not True = False
not False = True

class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  g /= h = not (g == h)

class Eq a => Ord a where
  (>=) :: a -> a -> Bool
  (<) :: a -> a -> Bool
  g < h = not (g >= h)

class Functor f where
  fmap :: {-(f @ a, f @ b) => -} (a -> b) -> f a -> f b

