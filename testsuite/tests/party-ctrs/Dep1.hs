{-# LANGUAGE DataKinds, PolyKinds, RankNTypes, TypeFamilies, TypeOperators #-}

module Dep1 where

import Data.Kind
-- import GHC.Types(type (@@))

data Proxy k (a :: k) = P

proxy :: Proxy Type Int
proxy = P

-- y :: Proxy Bool True
-- y = P

-- z :: Proxy Bool 'True
-- z = P


data Wacky k        -- kind              -- NamedTCB Required
           (a :: k) -- constrained type  -- AnnonTCB VisArg
           k'       -- kind              -- NamedTCB Required
           (b ::k') -- constrained type  -- AnnonTCB VisArg
  = W

newtype Const   -- {k1}        -- NamedTCB infered
                -- {k2}        -- NamedTCB infered
              a -- {a :: k1}   -- AnnonTCB VisArg
              b -- {b :: k2}   -- AnnonTCB VisArg
  = Const { getConst :: a }

data N = Z | S N

wacky :: Wacky Type N Type Bool
wacky = W
