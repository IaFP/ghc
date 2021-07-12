{-# LANGUAGE DeriveGeneric, DatatypeContexts #-}

module Deriving where

import GHC.Generics


-- We do not support datatypes with context
data (Show a) => Context a = Context a deriving (Generic1)
