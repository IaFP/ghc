{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1993-1998

-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Typechecking annotations
module GHC.Tc.Gen.Annotation ( tcAnnotations, annCtxt ) where

import GHC.Prelude

import GHC.Driver.Session
import GHC.Driver.Env

import GHC.Tc.Errors.Types
import {-# SOURCE #-} GHC.Tc.Gen.Splice ( runAnnotation )
import GHC.Tc.Utils.Monad

import GHC.Unit.Module

import GHC.Hs

import GHC.Utils.Outputable

import GHC.Types.Name
import GHC.Types.Annotations
import GHC.Types.SrcLoc
#if MIN_VERSION_base(4,16,0)
import GHC.Types (WDT)
#endif

import Control.Monad ( when )

-- Some platforms don't support the interpreter, and compilation on those
-- platforms shouldn't fail just due to annotations
tcAnnotations :: [LAnnDecl GhcRn] -> TcM [Annotation]
tcAnnotations anns = do
  hsc_env <- getTopEnv
  case hsc_interp hsc_env of
    Just _  -> mapM tcAnnotation anns
    Nothing -> warnAnns anns

warnAnns :: [LAnnDecl GhcRn] -> TcM [Annotation]
--- No GHCI; emit a warning (not an error) and ignore. cf #4268
warnAnns [] = return []
warnAnns anns@(L loc _ : _)
  = do { setSrcSpanA loc $ addDiagnosticTc (TcRnIgnoringAnnotations anns)
       ; return [] }

tcAnnotation :: LAnnDecl GhcRn -> TcM Annotation
tcAnnotation (L loc ann@(HsAnnotation _ _ provenance expr)) = do
    -- Work out what the full target of this annotation was
    mod <- getModule
    let target = annProvenanceToTarget mod provenance

    -- Run that annotation and construct the full Annotation data structure
    setSrcSpanA loc $ addErrCtxt (annCtxt ann) $ do
      -- See #10826 -- Annotations allow one to bypass Safe Haskell.
      dflags <- getDynFlags
      when (safeLanguageOn dflags) $ failWithTc TcRnAnnotationInSafeHaskell
      runAnnotation target expr

annProvenanceToTarget :: Module -> AnnProvenance GhcRn
                      -> AnnTarget Name
annProvenanceToTarget _   (ValueAnnProvenance (L _ name)) = NamedTarget name
annProvenanceToTarget _   (TypeAnnProvenance (L _ name))  = NamedTarget name
annProvenanceToTarget mod ModuleAnnProvenance             = ModuleTarget mod

annCtxt :: (
#if MIN_VERSION_base(4,16,0)
    WDT (XOverLit (GhcPass p))
  , WDT (XOverLit (GhcPass (NoGhcTcPass p)))
  , WDT (Anno (IdGhcP p))
  , WDT (Anno (IdGhcP (NoGhcTcPass p)))
  , WDT (SyntaxExprGhc p)
  , WDT (SyntaxExprGhc (NoGhcTcPass p))
  ,
#endif
  OutputableBndrId p) => AnnDecl (GhcPass p) -> SDoc
annCtxt ann
  = hang (text "In the annotation:") 2 (ppr ann)
