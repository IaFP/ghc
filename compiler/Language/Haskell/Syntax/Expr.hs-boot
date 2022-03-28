{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

-- See Note [Language.Haskell.Syntax.* Hierarchy] for why not GHC.Hs.*
module Language.Haskell.Syntax.Expr where

import Language.Haskell.Syntax.Extension ( XRec )
import Data.Kind  ( Type )

type role HsExpr nominal
type role MatchGroup nominal nominal
type role GRHSs nominal nominal
type role HsSplice nominal
type role HsCmd nominal
data HsExpr (i :: Type)
data HsCmd (i :: Type) -- need this for partial type families
data HsSplice (i :: Type)
data MatchGroup (a :: Type) (body :: Type)
data GRHSs (a :: Type) (body :: Type)
type family SyntaxExpr (i :: Type)

type LHsExpr a = XRec a (HsExpr a)
