{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module IndexedType where

import GHC.Types (type (@@), Total)
import Data.Kind (Type)

data P f g r = f r :*: g r
type family TrieMapT (f :: Type -> Type) ::
  Type -> (Type -> Type) -> Type -> Type

newtype (m2 k a) @@ ix => PMap m1 (m2 :: Type -> (Type -> Type) -> Type -> Type)
             k (a :: Type -> Type) ix
  = PMap (m1 k (m2 k a) ix)

type instance TrieMapT (P f g) = PMap (TrieMapT f) (TrieMapT g)

class TrieKeyT f m where
        unionT :: (m k a @@ ix, TrieMapT f ~ m) => (f k -> a ix -> a ix -> a ix) ->
                m k a ix -> m k a ix -> m k a ix
        sizeT :: (TrieMapT f ~ m) => m k a ix -> Int

instance (TrieKeyT f m1, TrieKeyT g m2) => TrieKeyT (P f g) (PMap m1 m2) where
        unionT f (PMap m1) (PMap m2) = PMap (uT  (\ a -> unionT (\ b -> f (a :*: b))) m1 m2)
                where uT = unionT
        sizeT = error "urk"

