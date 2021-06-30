{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuantifiedConstraints #-}

module T11552 where
import GHC.Types (Total)
newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) =
    MaybeT $ (fmap . fmap) f ma

instance (forall f . (Applicative f), Total f) => Applicative (MaybeT f) where
  pure :: a -> MaybeT f a
  pure x = MaybeT (pure (pure x))

  (<*>) :: forall a b . Applicative f => MaybeT f (a -> b) -> MaybeT f a -> MaybeT f b
  (MaybeT fab) <*> (MaybeT mma) =
   let fab' :: f (Maybe (a -> b))
       fab' = fab
   in MaybeT $ undefined
