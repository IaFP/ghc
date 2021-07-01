{-# LANGUAGE DataKinds, PolyKinds, RankNTypes, TypeFamilies #-}

module Dep1 where

import Data.Kind
import GHC.Types(Constraint)

data Proxy k (a :: k) = P

-- Proxy :: forall k. (a :: k) :: *
type family (@@) (t :: k' -> k) (u :: k') :: Constraint

-- type instance Proxy @@ k = ()
type instance Proxy k @@ a = ()

proxy :: (Proxy Type @@ Int) => Proxy Type Int
proxy = P

-- y :: Proxy Bool True
-- y = P

-- z :: Proxy Bool 'True
-- z = P


data Wacky k        -- kind
           (a :: k) -- constrained type
           k'       -- kind
           (b ::k') -- constrained type
  = W

-- here a can only have type of kind k and b to have kind k'

data N = Z | S N

wacky :: Wacky Type N Type Bool
wacky = W
