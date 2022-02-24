{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeConstructors, TypeOperators, UndecidableInstances, StandaloneDeriving #-}

module ClassElab where
import GHC.Types (type (@))

class Functr f where
  fmap' :: (a -> b) -> f a -> f b

class C a where
  i :: a -> a

data m @ a => ReaderT r m a = ReaderT (r -> m a)

class TransMonad t where
  blah :: m a -> t m a 

instance TransMonad (ReaderT r) where
  blah = undefined

instance C (ReaderT r m a) where
  i = id
