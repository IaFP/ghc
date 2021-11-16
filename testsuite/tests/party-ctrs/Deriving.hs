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

import Data.Void
import Data.Complex
import Data.Functor.Const
import Data.Functor.Identity
import Data.Ratio
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative hiding (WrappedMonad(..))

import Data.Bifunctor
import Data.Monoid
import Data.Kind
import GHC.Types (Total, type (@@))

type f ~> g = forall xx. f xx -> g xx

type MTrans = (Type -> Type) -> (Type -> Type)

-- From `constraints'
data Dict c where
  Dict :: c => Dict c

newtype a :- b = Sub (a => Dict b)

infixl 1 \\
(\\) :: a => (b => r) -> (a :- b) -> r
r \\ Sub Dict = r

-- With `-XQuantifiedConstraints' this just becomes
--
--    type Lifting cls  trans = forall mm. cls mm => cls (trans mm)
--
--    type LiftingMonad trans = Lifting Monad trans
--
class LiftingMonad (trans :: MTrans) where
  proof :: Monad m :- Monad (trans m)

instance LiftingMonad (StateT s :: MTrans) where
  proof :: (Monad m) :- Monad (StateT s m)
  proof = Sub Dict

instance Monoid w => LiftingMonad (WriterT w :: MTrans) where
  proof :: (Monad m) :- Monad (WriterT w m)
  proof = Sub Dict

instance (LiftingMonad trans, LiftingMonad trans')
         => LiftingMonad (ComposeT trans trans' :: MTrans) where
  proof :: forall m. (Monad m) :- Monad (ComposeT trans trans' m)
  proof = Sub (Dict \\ proof @trans @(trans' m) \\ proof @trans' @m)

newtype Stack :: MTrans where
  Stack :: ReaderT Int (StateT Bool (WriterT String m)) a -> Stack m a
  -- deriving newtype
    -- ( Functor
    -- , Applicative
    -- , Monad
    -- , MonadReader Int
    -- , MonadState Bool
    -- , MonadWriter String
    -- )
  -- deriving (MFunctor)
  --      via (ReaderT Int `ComposeT` StateT Bool `ComposeT` WriterT String)

deriving instance (Functor (Stack a))
deriving instance (Applicative (Stack a))
deriving instance (Monad (Stack a))
deriving instance (MonadReader Int (Stack a))
deriving instance (MonadState Bool (Stack a))
deriving instance (MonadWriter String (Stack a))

-- deriving instance (MonadTrans Stack)
-- deriving instance (MFunctor Stack)

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
  -- deriving newtype (Functor, Applicative, Monad)
deriving instance (Functor (ComposeT f g m))
deriving instance (Applicative (ComposeT f g m))
deriving instance (Monad (ComposeT f g m))

instance (MonadTrans f, MonadTrans g, LiftingMonad g) => MonadTrans (ComposeT f g) where
  lift :: forall m. Monad m => m ~> ComposeT f g m
  lift = ComposeT . lift . lift
    \\ proof @g @m

instance (MFunctor f, MFunctor g, LiftingMonad g) => MFunctor (ComposeT f g) where
  hoist :: forall m m'. Monad m => (m ~> m') -> (ComposeT f g m ~> ComposeT f g m')
  hoist f = ComposeT . hoist (hoist f) . getComposeT
    \\ proof @g @m
