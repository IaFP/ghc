{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE PartialTypeConstructors #-}

module Importer where
import GHC.Types (WFT)
import Exporter -- (Elem, G)

-- -- This should be given type
-- --   forall a. WF_Elem a => Elem a -> a
-- aaa :: Elem a -> a
-- aaa = undefined

-- -- -- -- This should have (Eq a, Eq b) constraint.
-- -- -- -- The difference from above is that Elem (a, b)
-- -- -- -- normalizes.
-- bbb :: Elem (a, b) -> (a, b)
-- bbb = undefined

-- -- -- This should also be given g's type from openTF.
-- ggg' :: Elem a -> a
-- ggg' = Exporter.g

-- -- -- WF_Elem should magically be in scope,
-- -- -- and should be given as annotation to g''.
-- ggg'' :: Elem a -> a
-- ggg'' = undefined

-- -- -- --------------------------------------------------------------------------------
-- -- -- -- Nullary TFs (checking if WF_G is exported)
-- -- -- --------------------------------------------------------------------------------

-- ccc :: G
-- ccc = undefined

-- c' :: WF_G =>G
-- c' = undefined


pprBlock :: (WFT (IndexedCO x SDoc SDoc), IndexedCO x SDoc SDoc ~ SDoc)
         => Platform -> Block CmmNode e x -> IndexedCO e SDoc SDoc
pprBlock platform block = undefined
