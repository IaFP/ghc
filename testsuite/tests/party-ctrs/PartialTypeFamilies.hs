{-# LANGUAGE CPP #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE PartialTypeConstructors #-}
#endif

{-# LANGUAGE TypeFamilies #-}

module PartialTypeFamilies where

#if __GLASGOW_HASKELL__ >= 810
import GHC.Types (type (@@))
#endif

data Ord a => Tree a = Leaf a | Node (Tree a) (Tree a)

type family F (a :: *) :: *
type instance F [a] = Tree a
type instance F Bool = Bool

-- Goal:
-- - generate type family _F (w/ unique name)
-- - Add instance _F [a] = BST @ a,
--   or _f [a] = mkConstraints where mkConstraints stolen from Apoorv code.


-- This *should* pass w/ PTC,
-- but fail w/o PTC.
f :: a -> F [a]
f = Leaf

-- This application should fail
-- w/ PTC.
-- g :: F [(a -> a)]
g = f id
