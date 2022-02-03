{-# LANGUAGE KindSignatures, PolyKinds, TypeOperators, TypeFamilies #-}
{-# LANGUAGE RankNTypes, FlexibleContexts , ConstrainedClassMethods #-}
{-# LANGUAGE PartialTypeConstructors, QuantifiedConstraints, DatatypeContexts
           , DefaultSignatures, ExistentialQuantification, ConstraintKinds, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass, MagicHash, UnboxedTuples, DataKinds, TypeApplications, UnliftedNewtypes #-}

module Stream where

-- | Intermediate result in a processing pipeline.
data Step s a = Done
              | Skip s
              | Yield a s

data Stream a =
    forall s. Stream
    (s -> Step s a)             -- stepper function
    s                          -- current state
    Size                       -- size hint in code units

data Size = Between Int Int -- ^ Lower and upper bounds on size.
          | Unknown
          deriving (Eq, Show)
