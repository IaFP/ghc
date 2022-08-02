{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances, TypeFamilyDependencies #-}

module ES where

import GHC.Types (Type, Total, type (@))

type Size = Int

class (Total m, Monad m) => PrimMonad m where
  type PrimState m

type family Mutable (v :: Type -> Type) = (mv :: Type -> Type -> Type) | mv -> v

class MVector v a 


data Step s a where
  Yield :: a -> s -> Step s a
  Skip  :: s -> Step s a
  Done  :: Step s a

data Monad m => Stream m a = forall s. (m @ Step s a) => Stream (s -> m (Step s a)) s

data Chunk v a = Chunk Int (forall m. (MVector (Mutable v) a, PrimMonad m) => Mutable v (PrimState m) a -> m ())

data (Total m, Monad m, v @ a) => Bundle m v a =
  Bundle { sVector :: Maybe (v a) }


munstream :: Bundle m u a -> m (v (PrimState m) a)
munstream = undefined

replicateM_inner :: Int -> m a -> Bundle m v a
replicateM_inner = undefined

replicateM :: Int -> m a -> m (v (PrimState m) a)
replicateM n m = munstream (replicateM_inner n m)
