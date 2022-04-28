{-# LANGUAGE KindSignatures, PolyKinds, TypeOperators, TypeFamilies #-}
{-# LANGUAGE RankNTypes, FlexibleContexts , ConstrainedClassMethods #-}
{-# LANGUAGE PartialTypeConstructors, QuantifiedConstraints, DatatypeContexts
           , DefaultSignatures, ExistentialQuantification, ConstraintKinds, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass, MagicHash, UnboxedTuples, DataKinds, TypeApplications, UnliftedNewtypes #-}

module RecursiveDTs where



data Queue e = Q e (QList e)
data QList e
    = Nil
    | QCons (Queue e) (QList e)

data Map a b = M a b
-- newtype Ord a => NTRec1 a = NTRec1 {unntRec1 :: NTRec2 a}
-- newtype Eq a => NTRec2 a = NTRec2 {unntRec2 :: NTRec1 a} -- This doesn't work

newtype Ord a => NTRec1 a = NTRec1 {unntRec1 :: NTRec2 a}
newtype Ord a => NTRec2 a = NTRec2 {unntRec2 :: NTRec1 a} -- This should work

data IsNode a => Graph a = Graph { graphMap :: (Map (Key a) a) }

class Ord (Key a) => IsNode a where
  type Key a :: *
  nodeKey :: a -> Key a
  nodeNeighbors :: a -> [Key a]
                
