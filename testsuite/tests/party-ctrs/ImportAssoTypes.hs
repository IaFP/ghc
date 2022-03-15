
{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module ImportAssoTypes where

import GHC.Types
import AssoTypes

-- blahType :: Elem a -> a
-- blahType = undefined


instance Collection [a] where
  type Elem [a] = a
  e = []
  cons = (:)


-- some higher order order assocated types

-- data Pair a b = Pair a b
-- data Ord a => OrdPair a b = OrdPair a b

-- instance Gen (Pair a b) where -- I don't know how to write this tbh i suspect i'm missing :*: things
--   type Repr (Pair a b) = (,) a
--   from (Pair a b) = (a, b)
--   to (a, b) = Pair a b


-- instance Gen (OrdPair a b) where
--   type Repr (OrdPair a b) = (,) a
--   from (OrdPair a b) = (a, b)
--   to (a, b) = OrdPair a b
