{-# LANGUAGE TypeFamilies, TypeFamilyDependencies, PartialTypeConstructors #-}

module Importer where

import OpenTF

-- This should be given type
--   forall a. WF_Elem a => Elem a -> a
g :: Elem a -> a
g = undefined

-- This should also be given g's type from openTF.
-- g' = OpenTF.g

-- WF_Elem should magically be in scope (if it is exported from OpenTF)
-- g'' :: WF_Elem a => Elem a -> a
-- g'' = OpenTF.g
