{-# LANGUAGE MagicHash, NoImplicitPrelude, TypeFamilies, UnboxedTuples,
             MultiParamTypeClasses, RoleAnnotations, CPP, TypeOperators,
             PolyKinds, NegativeLiterals, DataKinds, ScopedTypeVariables,
             TypeApplications, StandaloneKindSignatures,
             FlexibleInstances, UndecidableInstances,
             UndecidableSuperClasses, RankNTypes, ConstraintKinds, QuantifiedConstraints #-}
-- NegativeLiterals: see Note [Fixity of (->)]
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Types
-- Copyright   :  (c) The University of Glasgow 2009
-- License     :  see libraries/ghc-prim/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- GHC type definitions.
-- Use GHC.Exts from the base package instead of importing this
-- module directly.
--
-----------------------------------------------------------------------------

module Any where
{- *********************************************************************
*                                                                      *
                  Any
*                                                                      *
********************************************************************* -}

-- | The type constructor 'Any' is type to which you can unsafely coerce any
-- lifted type, and back. More concretely, for a lifted type @t@ and
-- value @x :: t@, -- @unsafeCoerce (unsafeCoerce x :: Any) :: t@ is equivalent
-- to @x@.
--
type family Any :: k where { }
