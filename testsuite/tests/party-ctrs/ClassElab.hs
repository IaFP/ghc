{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeConstructors, TypeOperators, UndecidableInstances, StandaloneDeriving #-}

module ClassElab where


class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

