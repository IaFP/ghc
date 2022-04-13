{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 903
{-# LANGUAGE TypeOperators, TypeFamilies #-}
#endif
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module Language.Haskell.Syntax.Extension

{-# OPTIONS_GHC -Wno-orphans #-} -- Outputable

module GHC.Hs.Expr where

import GHC.Utils.Outputable ( SDoc, Outputable )
import Language.Haskell.Syntax.Pat ( LPat )
import {-# SOURCE #-} GHC.Hs.Pat () -- for Outputable
import GHC.Types.Basic ( SpliceExplicitFlag(..))
import Language.Haskell.Syntax.Expr
  ( HsExpr, LHsExpr
  , HsCmd
  , MatchGroup
  , GRHSs
  , HsSplice
  )
import GHC.Hs.Extension ( OutputableBndrId, GhcPass )
#if MIN_VERSION_base(4,16,0)
import GHC.Types (WFT)
import Language.Haskell.Syntax.Extension
import GHC.Hs.Extension (NoGhcTcPass, IdGhcP)
#endif

instance (
#if MIN_VERSION_base(4,16,0)
  WFT (XOverLit (GhcPass p)),
  WFT (XOverLit (GhcPass (NoGhcTcPass p))),
  WFT (Anno (IdGhcP p)),
  WFT (Anno (IdGhcP (NoGhcTcPass p))),
  WFT (SyntaxExprGhc p),		
#endif
    OutputableBndrId p) => Outputable (HsExpr (GhcPass p))
instance (
#if MIN_VERSION_base(4,16,0)
  WFT (XOverLit (GhcPass p)),
  WFT (XOverLit (GhcPass (NoGhcTcPass p))),
  WFT (Anno (IdGhcP p)),
  WFT (Anno (IdGhcP (NoGhcTcPass p))),
  WFT (SyntaxExprGhc p),
#endif
  OutputableBndrId p) => Outputable (HsCmd (GhcPass p))

pprLExpr :: (
#if MIN_VERSION_base(4,16,0)
  WFT (XOverLit (GhcPass p)),
  WFT (XOverLit (GhcPass (NoGhcTcPass p))),
  WFT (Anno (IdGhcP p)),
  WFT (Anno (IdGhcP (NoGhcTcPass p))),
  WFT (SyntaxExprGhc p),
#endif
  OutputableBndrId p) => LHsExpr (GhcPass p) -> SDoc

pprExpr :: (
#if MIN_VERSION_base(4,16,0)
  WFT (XOverLit (GhcPass p)),
  WFT (XOverLit (GhcPass (NoGhcTcPass p))),
  WFT (Anno (IdGhcP p)),
  WFT (Anno (IdGhcP (NoGhcTcPass p))),
  WFT (SyntaxExprGhc p),
#endif
    OutputableBndrId p) => HsExpr (GhcPass p) -> SDoc

pprSplice :: (
#if MIN_VERSION_base(4,16,0)
  WFT (XOverLit (GhcPass p)),
  WFT (XOverLit (GhcPass (NoGhcTcPass p))),
  WFT (Anno (IdGhcP p)),
  WFT (Anno (IdGhcP (NoGhcTcPass p))),
  WFT (SyntaxExprGhc p),
#endif
  OutputableBndrId p) => HsSplice (GhcPass p) -> SDoc

pprSpliceDecl ::  (
#if MIN_VERSION_base(4,16,0)
  WFT (XOverLit (GhcPass p)),
  WFT (XOverLit (GhcPass (NoGhcTcPass p))),
  WFT (Anno (IdGhcP p)),
  WFT (Anno (IdGhcP (NoGhcTcPass p))),
  WFT (SyntaxExprGhc p),
#endif
  OutputableBndrId p)
          => HsSplice (GhcPass p) -> SpliceExplicitFlag -> SDoc

pprPatBind :: forall bndr p . (
#if MIN_VERSION_base(4,16,0)
  WFT (XOverLit (GhcPass p)),
  WFT (XOverLit (GhcPass (NoGhcTcPass p))),
  WFT (Anno (IdGhcP p)),
  WFT (Anno (IdGhcP (NoGhcTcPass p))),                      
  WFT (XOverLit (GhcPass bndr)),  
  WFT (XOverLit (GhcPass (NoGhcTcPass bndr))),
  WFT (Anno (IdGhcP bndr)),
  WFT (Anno (IdGhcP (NoGhcTcPass bndr))),
  WFT (SyntaxExprGhc p),
#endif
  OutputableBndrId bndr,
  OutputableBndrId p)
           => LPat (GhcPass bndr) -> GRHSs (GhcPass p) (LHsExpr (GhcPass p)) -> SDoc

pprFunBind :: (
#if MIN_VERSION_base(4,16,0)
  WFT (XOverLit (GhcPass idR)),
  WFT (XOverLit (GhcPass (NoGhcTcPass idR))),
  WFT (Anno (IdGhcP idR)),
  WFT (Anno (IdGhcP (NoGhcTcPass idR))),
  WFT (SyntaxExprGhc idR),
#endif
     OutputableBndrId idR)
           => MatchGroup (GhcPass idR) (LHsExpr (GhcPass idR)) -> SDoc
