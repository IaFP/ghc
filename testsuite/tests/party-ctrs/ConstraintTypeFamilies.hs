{-# LANGUAGE Unsafe, DatatypeContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators, TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}
{-# LANGUAGE PartialTypeConstructors, GADTs, DataKinds #-}


module ConstraintTypeFamilies where

import GHC.Types (Type)

type family Equ a b :: Bool where
  Equ a a = True
  Equ a b = False

type family Loop :: Type
type instance Loop = [Loop]

-- data F = Equ Loop [Loop]
-- data T = Equ Loop Loop
-- This fails, a
-- g :: Loop -> Loop
-- g x = x : x
