{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module T16312 where

import GHC.Types (Total)

newtype Curried g h a =
  Curried { runCurried :: forall r. g (a -> r) -> h r }

instance (Total g, Functor g, Total h) => Functor (Curried g h) where
  fmap f (Curried g) = Curried (g . fmap (.f))

instance (Total g, Functor g, g ~ h) => Applicative (Curried g h) where
  pure a = Curried (fmap ($a))
  Curried mf <*> Curried ma = Curried (ma . mf . fmap (.))
  {-# INLINE (<*>) #-}
