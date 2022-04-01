{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 903
{-# LANGUAGE QuantifiedConstraints, ExplicitNamespaces, TypeOperators, TypeFamilies #-}
#endif

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module Language.Haskell.Syntax.Extension

{-# OPTIONS_GHC -Wno-orphans #-} -- Outputable

module Pat (pprLPat) where

import TTGCommon
import PatSyntax
import GHC.Types

pprLPat :: LPat (GhcPass i) -> String
pprLPat = undefined

type instance Anno (Pat (GhcPass p)) = ()
                     
