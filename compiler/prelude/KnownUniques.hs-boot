{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE PartialTypeConstructors, TypeOperators, TypeFamilies #-}
{-# OPTIONS -fno-enable-rewrite-rules #-}
#endif
module KnownUniques where

import GhcPrelude
import Unique
import Name
import BasicTypes

-- Needed by TysWiredIn
knownUniqueName :: Unique -> Maybe Name

mkSumTyConUnique :: Arity -> Unique
mkSumDataConUnique :: ConTagZ -> Arity -> Unique

mkCTupleTyConUnique :: Arity -> Unique
mkCTupleDataConUnique :: Arity -> Unique

mkTupleTyConUnique :: Boxity -> Arity -> Unique
mkTupleDataConUnique :: Boxity -> Arity -> Unique
