{-# LANGUAGE CPP #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGAUGE ExplicitNamespaces #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances, QuantifiedConstraints #-}

module PartialDataTypes where
import GHC.Types (WDT, type (@), Total)


class (Total m, Monad m) => PrimMonad m where
  -- | State token type.
  type PrimState m

  -- | Execute a primitive operation.
  primitive :: (State# (PrimState m) -> (# State# (PrimState m), a #)) -> m a


data Chunk v a = Chunk Int (forall m. (M.MVector (Mutable v) a, PrimMonad m) => Mutable v (PrimState m) a -> m ())

-- | Monadic streams
data (Total m, Monad m) => Bundle m v a =
  Bundle { sElems  :: Stream m a
         , sChunks :: Stream m (Chunk v a)
         , sVector :: v @ a => Maybe (v a)
         , sSize   :: Size
         }
