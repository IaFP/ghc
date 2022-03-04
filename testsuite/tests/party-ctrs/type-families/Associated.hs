{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeConstructors, DataKinds #-}

module Associated where
import GHC.Types
import Data.Proxy
import GHC.TypeLits

class Collection a where
  type Elem a
  e :: a
  cons :: Elem a -> a -> a


instance Collection [a] where
  type Elem [a] = a
  e = []
  cons = (:)


-- some higher order order assocated types

class Gen a where
  type Repr a :: Type -> Type
  -- WFT (Repr a) :: Type -> Constraint
  from :: a -> (Repr a) x
  to :: (Repr a) x -> a


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
