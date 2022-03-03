{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE PartialTypeConstructors #-}

module Associated where

import GHC.Types

class Collection a where
  type family Elem a
  type family ElemWF a :: Constraint
  e :: a
  cons :: ElemWF a => Elem a -> a -> a


-- instance Collection [a] where
--   type instance Elem [a] = a
--   e = []
--   cons = (:)


class Cls t a where
  methBlah :: t a -> a
