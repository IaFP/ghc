{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE PartialTypeConstructors #-}

module Exporter (
  Block (..),
  IndexedCO(..),
  CmmNode, SDoc, O, C, Extensibility (..), Elem, Platform
  ) where

import GHC.Types (Type)

type family Elem a

foobar :: Int -> Int
foobar = id

-- Should elaborate to constraint
--   WF_Elem [a] = ()
type instance Elem [a] = a

data (Eq a, Eq b) => EqPair a b = EqPair (a, b)

-- Should elaborate to constraint
--   WF_Elem (a, b) = (Eq a, Eq b)
type instance Elem (a, b) = EqPair a b

-- Should be given constraint
--   WF_Elem a => ...
-- as "Elem a" does not normalize.
g :: Elem a -> a
g = undefined


data Ord a => T a = L | N a (T a) (T a)
type instance (Elem (T a)) = a

data Extensibility
  = Open
  | Closed

type O = 'Open
type C = 'Closed

-- | Either type indexed by closed/open using type families
type family IndexedCO (ex :: Extensibility) (a :: k) (b :: k) :: k
type instance IndexedCO C a _b = a
type instance IndexedCO O _a b = b

data SDoc
data Platform
data CmmNode e x

data Block n e x where
  BlockCO  :: n C O -> Block n O O          -> Block n C O
  BlockCC  :: n C O -> Block n O O -> n O C -> Block n C C
  BlockOC  ::          Block n O O -> n O C -> Block n O C

  BNil    :: Block n O O
  BMiddle :: n O O                      -> Block n O O
  BCat    :: Block n O O -> Block n O O -> Block n O O
  BSnoc   :: Block n O O -> n O O       -> Block n O O
  BCons   :: n O O       -> Block n O O -> Block n O O


--------------------------------------------------------------------------------
-- What about nullary TFs?
--------------------------------------------------------------------------------

type family G


