{-# LANGUAGE KindSignatures, PolyKinds, TypeOperators, TypeFamilies #-}
{-# LANGUAGE RankNTypes, FlexibleContexts , ConstrainedClassMethods #-}
{-# LANGUAGE PartialTypeConstructors, QuantifiedConstraints, DatatypeContexts
           , DefaultSignatures, ExistentialQuantification, ConstraintKinds, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass, MagicHash, UnboxedTuples, DataKinds, TypeApplications, UnliftedNewtypes #-}

module RecursiveDTs where


-- import GHC.Types (Constraint)
import GHC.Types (type (@@), Total)


data Queue e = Q e (QList e)
data QList e
    = Nil
    | QCons (Queue e) (QList e)



data ListMap m a
  = LM { lm_nil  :: Maybe a
       , lm_cons :: m (ListMap m a) }

class TrieMap m where
   type Key m :: *
   emptyTM  :: m a

type BndrMap = TypeMapG

data CoreMapX a = R {bleh :: ListMap CoreMapG (CoreMapG (ListMap BndrMap a))}

type TypeMapG = GenMap TypeMapX

type CoreMapG = GenMap CoreMapX

data GenMap m a
   = EmptyMap
   | SingletonMap (Key m) a
   | MultiMap (m a)

data TypeMapX a
  = TM { tm_app :: TypeMapG (TypeMapG a) }

emptyE :: CoreMapX a
emptyE = undefined


-- CoreMapX @@ a ~> ListMap CoreMapG @@ CoreMapG (ListMap BndrMap a)
--               ~> GenMap CoreMapX @@ ListMap (GenMap CoreMapX) (GenMap CoreMapX (ListMap BndrMap a))
--               ~> CoreMapX @@ ListMap (GenMap CoreMapX) (GenMap CoreMapX (ListMap BndrMap a))

-- Yikes!
