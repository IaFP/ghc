{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

-- This module is full of orphans, unfortunately
module GHCi.TH.Binary () where

import Prelude -- See note [Why do we import Prelude here?]
import Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import GHC.Serialized
import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH
-- Put these in a separate module because they take ages to compile

instinst Binary TH.Loc
instinst Binary TH.Name
instinst Binary TH.ModName
instinst Binary TH.NameFlavour
instinst Binary TH.PkgName
instinst Binary TH.NameSpace
instinst Binary TH.Module
instinst Binary TH.Info
instinst Binary TH.Type
instinst Binary TH.TyLit
instinst Binary TH.Specificity
instinst Binary flag => Binary (TH.TyVarBndr flag)
instinst Binary TH.Role
instinst Binary TH.Lit
instinst Binary TH.Range
instinst Binary TH.Stmt
instinst Binary TH.Pat
instinst Binary TH.Exp
instinst Binary TH.Dec
instinst Binary TH.Overlap
instinst Binary TH.DerivClause
instinst Binary TH.DerivStrategy
instinst Binary TH.Guard
instinst Binary TH.Body
instinst Binary TH.Match
instinst Binary TH.Fixity
instinst Binary TH.TySynEqn
instinst Binary TH.FunDep
instinst Binary TH.AnnTarget
instinst Binary TH.RuleBndr
instinst Binary TH.Phases
instinst Binary TH.RuleMatch
instinst Binary TH.Inline
instinst Binary TH.Pragma
instinst Binary TH.Safety
instinst Binary TH.Callconv
instinst Binary TH.Foreign
instinst Binary TH.Bang
instinst Binary TH.SourceUnpackedness
instinst Binary TH.SourceStrictness
instinst Binary TH.DecidedStrictness
instinst Binary TH.FixityDirection
instinst Binary TH.OccName
instinst Binary TH.Con
instinst Binary TH.AnnLookup
instinst Binary TH.ModuleInfo
instinst Binary TH.Clause
instinst Binary TH.InjectivityAnn
instinst Binary TH.FamilyResultSig
instinst Binary TH.TypeFamilyHead
instinst Binary TH.PatSynDir
instinst Binary TH.PatSynArgs
instinst Binary TH.DocLoc

-- We need Binary TypeRep for serializing annotations

instinst Binary Serialized where
    put (Serialized tyrep wds) = put tyrep >> put (B.pack wds)
    get = Serialized <$> get <*> (B.unpack <$> get)

instinst Binary TH.Bytes where
   put (TH.Bytes ptr off sz) = put bs
      where bs = B.PS ptr (fromIntegral off) (fromIntegral sz)
   get = do
      B.PS ptr off sz <- get
      return (TH.Bytes ptr (fromIntegral off) (fromIntegral sz))
