{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash,RoleAnnotations #-}

module DataFamilies where

import Data.Proxy
import Data.Kind

class C s
data Symbol
type role Ptr phantom
data Ptr a
data Addr#
data Char#

data Tree a

data family DF a b

data instance Ord a => DF a b = L a b | B a (DF a b) (DF a b)
  deriving Functor

-- data instance DF (Tree a) b = Nil | Cons (Tree a) (DF (Tree a) b)
