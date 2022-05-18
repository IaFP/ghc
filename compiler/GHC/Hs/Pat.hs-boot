{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 903
{-# LANGUAGE QuantifiedConstraints, ExplicitNamespaces, TypeOperators, TypeFamilies, DataKinds #-}
#endif

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module Language.Haskell.Syntax.Extension

{-# OPTIONS_GHC -Wno-orphans #-} -- Outputable

module GHC.Hs.Pat where

import GHC.Utils.Outputable
import GHC.Hs.Extension (
#if MIN_VERSION_base(4,16,0)
  NoGhcTcPass, GhcRn, IdGhcP,
#endif
  OutputableBndrId, GhcPass )

import Language.Haskell.Syntax.Pat
#if MIN_VERSION_base(4,16,0)
import GHC.Types (WDT)
import Language.Haskell.Syntax.Extension
import {-# Source #-} Language.Haskell.Syntax.Expr (HsExpr)
import {-# SOURCE #-} GHC.Hs.Expr ( SyntaxExprGhc )
#endif

instance (
#if MIN_VERSION_base(4,16,0)
  WDT (Anno (HsExpr GhcRn)),
  WDT (Anno (HsExpr (GhcPass p))),
  WDT (SyntaxExprGhc p),
  WDT (SyntaxExprGhc (NoGhcTcPass p)),
#endif
  OutputableBndrId p) => Outputable (Pat (GhcPass p))

pprLPat :: (
#if MIN_VERSION_base(4,16,0)
  WDT (Anno (HsExpr GhcRn)),
  WDT (Anno (HsExpr (GhcPass p))),  
  WDT (SyntaxExprGhc p),
  WDT (SyntaxExprGhc (NoGhcTcPass p)),
#endif
  OutputableBndrId p) => LPat (GhcPass p) -> SDoc

