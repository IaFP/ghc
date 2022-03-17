{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Data.Type.Equality where
  
import Data.Maybe
import GHC.Enum
import GHC.Show
import GHC.Read
import GHC.Base
import Data.Type.Bool

data a :~: b where  -- See Note [The equality types story] in GHC.Builtin.Types.Prim
  Refl :: a :~: a
  
type (==) :: k -> k -> Bool

type family a == b where
  f a == g b = f == g && a == b
  a == a = 'True
  _ == _ = 'False

-- foobar :: ((Maybe Int == Maybe Float) :~: True)
-- foobar = undefined
