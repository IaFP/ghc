{-# LANGUAGE PartialTypeConstructors, ExplicitNamespaces, TypeOperators #-}
module Monad where


import Data.List.NonEmpty

class Applicative m => Monad' m where
  unit' :: a -> m a
  join' :: m (m a) -> m a


instance Monad' [] where
  unit' = pure
  join' = foldl (++) []

instance Monad' NonEmpty where
  unit' = pure
  join' ~((a :| as) :| tl) = (a :| (as ++ (foldl (++) [] $ fmap toList tl)))
    where toList ~(a :| as) = a:as


instance Monad NonEmpty where
  return = unit'
  m >>= f = join' $ fmap f m
