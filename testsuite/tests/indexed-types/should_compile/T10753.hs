{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints -fno-warn-missing-methods #-}
module T10753 where

import Data.Kind (Type)
import GHC.Types (Total)

class MonadState s m | m -> s where
  get :: m s

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }
instance (Total m, Monad m) => Monad (StateT s m) where
instance (Total m, Functor m, Monad m) => Applicative (StateT s m) where
instance (Functor m) => Functor (StateT s m) where

instance (Monad m) => MonadState s (StateT s m) where

class HasConns (m :: Type -> Type) where
    type Conn m

foo :: (Monad m, Total m) => StateT (Conn m) m ()
foo =
    do _ <- get
       return ()
