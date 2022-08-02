{-# LANGUAGE PartialTypeConstructors, ExplicitNamespaces, TypeOperators #-}
module Monoidal where


class Functor f => Monoidal f where
  unit :: f ()
  pure :: a -> f a
  (>*<) :: f a -> f b -> f (a, b)


class Functor f => Applicative f where
  pure :: a -> f a
  <*> :: f (a -> b) -> f a -> f b

fap :: Monoidal f => f (a -> b, a) -> f b
fap = fmap ($)

liftA' :: Monoidal f => (a -> b) -> f a -> f b
liftA' f fa = ap ((pure f) >*< fa)

liftA2' :: Monoidal f => (a -> b -> c) -> f a -> f b -> f c


class Monoidal m => Monad' m where
  return' :: a -> m a
  return' = pure

  bind :: m a -> (a -> m b) -> m b
  
