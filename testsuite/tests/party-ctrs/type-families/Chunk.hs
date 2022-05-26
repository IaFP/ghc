{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}

module OpenTF3 where

import GHC.Types (WDT, Type, type(@))

class Monad m => PrimMonad m where
  type PrimState m

type family Mutable (v :: Type -> Type) = (mv :: Type -> Type -> Type) | mv -> v
  
data Chunk v a = Chunk Int (forall m. (m @ (), WDT (PrimState m), PrimMonad m) => Mutable v (PrimState m) a -> m ())

-- needsPrimStateDF :: m (PrimState m) a -> m ()
-- needsPrimStateDF = undefined

  
data Step s a where
  Yield :: a -> s -> Step s a
  Skip  :: s -> Step s a
  Done  :: Step s a

data ST m a = ST m a

needsPrimstate :: PrimMonad m => Mutable v (PrimState m) a -> m ()
needsPrimstate = undefined

vstep :: (PrimMonad m, WDT (Mutable v)) => s -> m (Step s (Chunk v a))
vstep s = return (Yield (Chunk 0 (\mv -> needsPrimstate mv)) s)
