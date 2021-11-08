{-# LANGUAGE CPP, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module GHC.Hs.PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE NoPartialTypeConstructors, TypeOperators #-}
#endif

module GHC.Hs.Expr where

import SrcLoc     ( Located )
import Outputable ( SDoc, Outputable )
import {-# SOURCE #-} GHC.Hs.Pat  ( LPat )
import BasicTypes ( SpliceExplicitFlag(..))
import GHC.Hs.Extension ( OutputableBndrId, GhcPass )

#if __GLASGOW_HASKELL__ >= 810
import GHC.Types (type (@@))
#endif


type role HsExpr nominal
type role HsCmd nominal
type role MatchGroup nominal nominal
type role GRHSs nominal nominal
type role HsSplice nominal
type role SyntaxExpr nominal
data HsExpr (i :: *)
data HsCmd  (i :: *)
data HsSplice (i :: *)
data MatchGroup (a :: *) (body :: *)
data GRHSs (a :: *) (body :: *)
data SyntaxExpr (i :: *)

instance OutputableBndrId p => Outputable (HsExpr (GhcPass p))
instance OutputableBndrId p => Outputable (HsCmd (GhcPass p))

type LHsExpr a = Located (HsExpr a)

pprLExpr :: (OutputableBndrId p
#if __GLASGOW_HASKELL__ >= 810
 -- We do this becuase GHC doesn't support type family instances in boot files
            , GhcPass @@ p
#endif
            ) => LHsExpr (GhcPass p) -> SDoc

pprExpr :: (OutputableBndrId p
#if __GLASGOW_HASKELL__ >= 810
           , GhcPass @@ p
#endif
           ) => HsExpr (GhcPass p) -> SDoc

pprSplice :: (OutputableBndrId p
#if __GLASGOW_HASKELL__ >= 810
             , GhcPass @@ p             
#endif
             ) => HsSplice (GhcPass p) -> SDoc

pprSpliceDecl ::  (OutputableBndrId p
#if __GLASGOW_HASKELL__ >= 810
                  , GhcPass @@ p
#endif
                  )
          => HsSplice (GhcPass p) -> SpliceExplicitFlag -> SDoc

pprPatBind :: forall bndr p body. (OutputableBndrId bndr,
                                   OutputableBndrId p,
                                   Outputable body
                                  )
           => LPat (GhcPass bndr) -> GRHSs (GhcPass p) body -> SDoc

pprFunBind :: forall idR body. (OutputableBndrId idR, Outputable body
-- #if __GLASGOW_HASKELL__ >= 810
--               , GhcPass @@ idR
--               , MatchGroup @@ GhcPass idR, MatchGroup (GhcPass idR) @@ body
-- #endif
              ) => MatchGroup (GhcPass idR) body -> SDoc
