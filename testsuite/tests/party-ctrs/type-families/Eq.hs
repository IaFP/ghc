{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NoImplicitPrelude        #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Trustworthy              #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}


module Equality where
  
import Data.Maybe
import GHC.Base
-- import Bool
import Data.Type.Bool hiding ((&&))

-- type family a && b where
--   'False && a      = 'False
--   'True  && a      = a
--   a      && 'False = 'False
--   a      && 'True  = a
--   a      && a      = a
-- infixr 3 &&

data a :~: b where  -- See Note [The equality types story] in GHC.Builtin.Types.Prim
  Refl :: a :~: a
  
type (==) :: k -> k -> Bool

type family a == b where
  f a == g b = f == g && a == b
  a == a = 'True
  _ == _ = 'False

foobar :: ((Maybe Int == Maybe Float) :~: True)
foobar = undefined
