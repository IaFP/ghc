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


class SingKind k where
  type DemoteRep k :: Type
  fromSing :: Sing (a :: k) -> DemoteRep k

instance SingKind Symbol where
  type DemoteRep Symbol = String
  fromSing (SSym :: Sing s) = symbolVal (Proxy :: Proxy s)

data family Sing (a :: k)

data SSymbol :: Symbol -> Type where
  SSym :: KnownSymbol s => SSymbol s
