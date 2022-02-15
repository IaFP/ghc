{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 903
{-# LANGUAGE QuantifiedConstraints, ExplicitNamespaces, TypeOperators #-}
#endif
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- BlockId module should probably go away completely, being superseded by Label -}
module GHC.Cmm.BlockId
  ( BlockId, mkBlockId -- ToDo: BlockId should be abstract, but it isn't yet
  , newBlockId
  , blockLbl, infoTblLbl
  ) where

import GHC.Prelude

import GHC.Cmm.CLabel
import GHC.Types.Id.Info
import GHC.Types.Name
import GHC.Types.Unique
import GHC.Types.Unique.Supply
#if MIN_VERSION_base(4,16,0)
import GHC.Types (type(@))
#endif

import GHC.Cmm.Dataflow.Label (Label, mkHooplLabel)

----------------------------------------------------------------
--- Block Ids, their environments, and their sets

{- Note [Unique BlockId]
~~~~~~~~~~~~~~~~~~~~~~~~
Although a 'BlockId' is a local label, for reasons of implementation,
'BlockId's must be unique within an entire compilation unit.  The reason
is that each local label is mapped to an assembly-language label, and in
most assembly languages allow, a label is visible throughout the entire
compilation unit in which it appears.
-}

type BlockId = Label

mkBlockId :: Unique -> BlockId
mkBlockId unique = mkHooplLabel $ getKey unique

newBlockId :: (
#if MIN_VERSION_base(4,16,0)
  m @ Unique, 
#endif
  MonadUnique m) => m BlockId
newBlockId = mkBlockId <$> getUniqueM

blockLbl :: BlockId -> CLabel
blockLbl label = mkLocalBlockLabel (getUnique label)

infoTblLbl :: BlockId -> CLabel
infoTblLbl label
  = mkBlockInfoTableLabel (mkFCallName (getUnique label) "block") NoCafRefs
