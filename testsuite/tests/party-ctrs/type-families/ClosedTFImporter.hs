{-# LANGUAGE PartialTypeConstructors #-}

module ClosedTFImporter where

import ClosedTF1 as CF1
import ClosedTF2 as CF2

--------------------------------------------------------------------------------
-- ClosedTF1
--------------------------------------------------------------------------------

g :: a -> CF1.F a
g = undefined

g' :: a -> CF1.F a
g' = CF1.f

--------------------------------------------------------------------------------
-- CLosedTF2
--------------------------------------------------------------------------------


h  :: a -> CF2.F a
h = undefined

h' :: Int -> CF2.F Int
h' = CF2.g'


h'' :: Int -> CF2.F (Int, Int)
h'' = CF2.g''

