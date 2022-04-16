{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

module ClosedBoot2 where

import ClosedBoot1

type family G a where
  G a = F a

