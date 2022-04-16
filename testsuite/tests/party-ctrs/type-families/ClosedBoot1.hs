{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

module ClosedBoot1 where

import {-# SOURCE #-} ClosedBoot2

type family F a where
  F a = G a

