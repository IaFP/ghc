{-# LANGUAGE UndecidableInstances #-}

module T9892 where

import Control.Applicative
import Control.Category
import Prelude hiding ((.),id)
import GHC.Types (Total)

newtype FocusingPlus w k s a = FocusingPlus { unfocusingPlus :: k (s, w) a }

instance Functor (k (s, w)) => Functor (FocusingPlus w k s) where
  fmap f (FocusingPlus as) = FocusingPlus (fmap f as)

instance Applicative (k (s, w)) => Applicative (FocusingPlus w k s) where
  pure = FocusingPlus . pure
  FocusingPlus kf <*> FocusingPlus ka = FocusingPlus (kf <*> ka)
  liftA2 f (FocusingPlus ka) (FocusingPlus kb) = FocusingPlus (liftA2 f ka kb)
