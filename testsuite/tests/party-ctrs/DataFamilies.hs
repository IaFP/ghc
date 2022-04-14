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
-- | The singleton kind-indexed data family.
data family Sing (a :: k)

data instance Sing (b :: Maybe a) where
  SNothing :: Sing 'Nothing
  SJust    :: Sing a -> Sing ('Just a)
  
data instance Sing (a :: Bool) where
  STrue  :: Sing 'True
  SFalse :: Sing 'False


data family URec (a :: Type) (p :: k)

data instance URec (Ptr ()) (p :: k) = UAddr { uAddr# :: Addr# }
data instance URec Char (p :: k) = UChar { uChar# :: Char# }

data Tree a

data family DF a

data instance DF Int = L Int | B Int (DF Int) (DF Int)

data instance DF (Tree a) = Nil | Cons (Tree a) (DF (Tree a))


