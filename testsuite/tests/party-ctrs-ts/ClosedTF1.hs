{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeConstructors #-}

module ClosedTF1 where

import GHC.Types (Type)

data Ord a => Tree a = Leaf a | Node (Tree a) (Tree a)

-- Since closed type families can permit overlapping
-- instances, so must WF_F constraints permit
-- overlapping instances.
type family F (a :: Type) :: Type where
  F [a] = Tree a
  F  a  = Bool


