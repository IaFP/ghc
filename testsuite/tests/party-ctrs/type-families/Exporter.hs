{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeConstructors #-}

module Exporter where

import GHC.Types (Type)

type family Elem a

foobar :: Int -> Int
foobar = id

-- Should elaborate to constraint
--   WF_Elem [a] = ()
type instance Elem [a] = a

data EqPair a b = EqPair (a, b)

-- Should elaborate to constraint
--   WF_Elem (a, b) = (Eq a, Eq b)
type instance Elem (a, b) = EqPair a b

-- Should be given constraint
--   WF_Elem a => ...
-- as "Elem a" does not normalize.
g :: Elem a -> a
g = undefined

--------------------------------------------------------------------------------
-- What about nullary TFs?
--------------------------------------------------------------------------------

type family G
