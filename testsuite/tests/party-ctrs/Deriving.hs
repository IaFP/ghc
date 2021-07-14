{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DatatypeContexts, PartialTypeConstructors, TypeOperators, TypeFamilies #-}

module Deriving where
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative hiding (WrappedMonad(..))
import Data.Monoid
import Data.Kind
import GHC.Types (type (@@), Total)

type MTrans = (Type -> Type) -> (Type -> Type)

type f ~> g = forall xx. (Total f, Total g) => f xx -> g xx


class MFunctor (trans :: MTrans) where
  hoist :: Monad m => (m ~> m') -> (trans m ~> trans m')

instance MFunctor (ReaderT r :: MTrans) where
  hoist :: Monad m => (m ~> m') -> (ReaderT r m ~> ReaderT r m')
  hoist nat = ReaderT . fmap nat . runReaderT

instance MFunctor (StateT s :: MTrans) where
  hoist :: Monad m => (m ~> m') -> (StateT s m ~> StateT s m')
  hoist nat = StateT . fmap nat . runStateT

instance MFunctor (WriterT w :: MTrans) where
  hoist :: Monad m => (m ~> m') -> (WriterT w m ~> WriterT w m')
  hoist nat = WriterT . nat . runWriterT



infixr 9 `ComposeT`
newtype ComposeT :: MTrans -> MTrans -> MTrans where
  ComposeT :: { getComposeT :: f (g m) a } -> ComposeT f g m a
  deriving newtype (Functor, Applicative, Monad)

newtype Stack :: MTrans where
  Stack :: ReaderT Int (StateT Bool (WriterT String m)) a -> Stack m a
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader Int
    , MonadState Bool
    , MonadWriter String
    )
  deriving (MonadTrans, MFunctor)
       via (ReaderT Int `ComposeT` StateT Bool `ComposeT` WriterT String)

