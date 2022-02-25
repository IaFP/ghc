{-# LANGUAGE TypeFamilies, PartialTypeConstructors #-}

module Importer where

import Exporter

-- This should be given type
--   forall a. WF_Elem a => Elem a -> a
a :: Elem a -> a
a = undefined

-- This should have (Eq a, Eq b) constraint.
-- The difference from above is that Elem (a, b)
-- normalizes.
b :: Elem (a, b) -> (a, b)
b (EqPair (a, b)) = (a, b)

-- This should also be given g's type from openTF.
-- g' = Exporter.g

-- WF_Elem should magically be in scope (if it is exported from OpenTF)
-- g'' :: WF_Elem a => Elem a -> a
-- g'' = undefined

--------------------------------------------------------------------------------
-- Nullary TFs (checking if WF_G is exported)
--------------------------------------------------------------------------------

c :: G
c = undefined

-- c' :: WF_G
-- c' = undefined
