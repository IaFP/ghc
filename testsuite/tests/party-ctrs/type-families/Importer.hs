{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeConstructors #-}

module Importer where

import Exporter

-- This should be given type
--   forall a. WF_Elem a => Elem a -> a
a :: Elem a -> a
a = undefined

-- -- -- This should have (Eq a, Eq b) constraint.
-- -- -- The difference from above is that Elem (a, b)
-- -- -- normalizes.
b :: Elem (a, b) -> (a, b)
b = undefined

-- -- This should also be given g's type from openTF.
-- g' :: Elem a -> a
-- g' = Exporter.g

-- -- WF_Elem should magically be in scope,
-- -- and should be given as annotation to g''.
g'' :: Elem a -> a
g'' = undefined

-- -- --------------------------------------------------------------------------------
-- -- -- Nullary TFs (checking if WF_G is exported)
-- -- --------------------------------------------------------------------------------

c :: G
c = undefined

-- c' :: WF_G => G
-- c' = undefined
