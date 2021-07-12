{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module T14578 where

import Control.Applicative
import Data.Functor.Compose
import Data.Semigroup
import GHC.Types (type (@@))

newtype App f a = MkApp (f a)
  deriving (Functor, Applicative)

instance (f @@ a, Applicative f, Semigroup a) => Semigroup (App f a) where
  (<>) = liftA2 (<>)

newtype Wat f g a = MkWat (App (Compose f g) a)
  deriving Semigroup
