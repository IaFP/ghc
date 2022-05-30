{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 903
{-# LANGUAGE TypeOperators, TypeFamilies, ConstraintKinds #-}
{-# LANGUAGE DataKinds         #-}  
#endif
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module Language.Haskell.Syntax.Extension

{-# OPTIONS_GHC -Wno-orphans #-} -- Outputable

module GHC.Hs.Expr where

import GHC.Utils.Outputable ( SDoc, Outputable )
import Language.Haskell.Syntax.Pat ( LPat )
-- import {-# SOURCE #-} GHC.Hs.Pat () -- for Outputable
import GHC.Types.Basic ( SpliceExplicitFlag(..))
import GHC.Tc.Types.Evidence ( HsWrapper )

import Language.Haskell.Syntax.Expr
  ( HsExpr, LHsExpr
  , HsCmd
  , MatchGroup
  , GRHSs
  , HsSplice
  )
import GHC.Hs.Extension ( OutputableBndrId, GhcPass, GhcRn, GhcTc
#if MIN_VERSION_base(4,16,0)
                        , Pass (..)
#endif
                        )

#if MIN_VERSION_base(4,16,0)
import GHC.Types (WDT)
import Language.Haskell.Syntax.Extension
import GHC.Hs.Extension (NoGhcTcPass)
import qualified Data.Kind
#endif

instance (
#if MIN_VERSION_base(4,16,0)
  WDT (SyntaxExprGhc p),
  WDT (SyntaxExprGhc (NoGhcTcPass p)),
#endif
    OutputableBndrId p) => Outputable (HsExpr (GhcPass p))
instance (
#if MIN_VERSION_base(4,16,0)
  WDT (SyntaxExprGhc p),
  WDT (SyntaxExprGhc (NoGhcTcPass p)),
#endif
  OutputableBndrId p) => Outputable (HsCmd (GhcPass p))

pprLExpr :: (
#if MIN_VERSION_base(4,16,0)
  WDT (SyntaxExprGhc p),
  WDT (SyntaxExprGhc (NoGhcTcPass p)),
#endif
  OutputableBndrId p) => LHsExpr (GhcPass p) -> SDoc

pprExpr :: (
#if MIN_VERSION_base(4,16,0)
  WDT (SyntaxExprGhc p),
  WDT (SyntaxExprGhc (NoGhcTcPass p)),
#endif
    OutputableBndrId p) => HsExpr (GhcPass p) -> SDoc

pprSplice :: (
#if MIN_VERSION_base(4,16,0)
  WDT (SyntaxExprGhc p),
  WDT (SyntaxExprGhc (NoGhcTcPass p)),
#endif
  OutputableBndrId p) => HsSplice (GhcPass p) -> SDoc

pprSpliceDecl ::  (
#if MIN_VERSION_base(4,16,0)
  WDT (SyntaxExprGhc p),
  WDT (SyntaxExprGhc (NoGhcTcPass p)),
#endif
  OutputableBndrId p)
          => HsSplice (GhcPass p) -> SpliceExplicitFlag -> SDoc

pprPatBind :: forall bndr p . (
#if MIN_VERSION_base(4,16,0)
  WDT (SyntaxExprGhc p),
  WDT (SyntaxExprGhc bndr),
  WDT (SyntaxExprGhc (NoGhcTcPass bndr)),
  WDT (SyntaxExprGhc (NoGhcTcPass p)),
#endif
  OutputableBndrId bndr,
  OutputableBndrId p)
           => LPat (GhcPass bndr) -> GRHSs (GhcPass p) (LHsExpr (GhcPass p)) -> SDoc

pprFunBind :: (
#if MIN_VERSION_base(4,16,0)
  WDT (SyntaxExprGhc idR),
  WDT (SyntaxExprGhc (NoGhcTcPass idR)),
#endif
     OutputableBndrId idR)
           => MatchGroup (GhcPass idR) (LHsExpr (GhcPass idR)) -> SDoc

data SyntaxExprRn = SyntaxExprRn (HsExpr GhcRn)
                  | NoSyntaxExprRn
#if MIN_VERSION_base(4,16,0)
type family SyntaxExprGhc (p :: Pass) = (r :: Data.Kind.Type) | r -> p where
  SyntaxExprGhc 'Parsed      = NoExtField
  SyntaxExprGhc 'Renamed     = SyntaxExprRn
  SyntaxExprGhc 'Typechecked = SyntaxExprTc
#endif
data SyntaxExprTc = SyntaxExprTc { syn_expr      :: HsExpr GhcTc
                                 , syn_arg_wraps :: [HsWrapper]
                                 , syn_res_wrap  :: HsWrapper }
                  | NoSyntaxExprTc  -- See Note [NoSyntaxExpr]

