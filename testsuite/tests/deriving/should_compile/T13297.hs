{-# Language TypeFamilies, StandaloneDeriving, GeneralizedNewtypeDeriving, UndecidableInstances #-}
module T13297 where

import Data.Kind (Type)
import GHC.Types (Total)

newtype N p m a = N (((CT p) m) a)
deriving instance (CT p ~ f, Functor (f m), Total (f m)) => Functor (N p m)
deriving instance (CT p ~ f, Applicative (f m), Total (f m)) => Applicative (N p m) -- panic when this line added

class C p where
    type CT p :: (Type -> Type) -> Type -> Type
