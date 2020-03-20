-----------------------------------------------------------------------------
--
-- Code generation for coverage
--
-- (c) Galois Connections, Inc. 2006
--
-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE PartialTypeConstructors, TypeOperators, TypeFamilies #-}
#endif

module GHC.StgToCmm.Hpc ( initHpc, mkTickBox ) where

import GhcPrelude

import GHC.StgToCmm.Monad

import MkGraph
import CmmExpr
import CLabel
import Module
import CmmUtils
import GHC.StgToCmm.Utils
import HscTypes
import DynFlags

import Control.Monad

mkTickBox :: DynFlags -> Module -> Int -> CmmAGraph
mkTickBox dflags mod n
  = mkStore tick_box (CmmMachOp (MO_Add W64)
                                [ CmmLoad tick_box b64
                                , CmmLit (CmmInt 1 W64)
                                ])
  where
    tick_box = cmmIndex dflags W64
                        (CmmLit $ CmmLabel $ mkHpcTicksLabel $ mod)
                        n

initHpc :: Module -> HpcInfo -> FCode ()
-- Emit top-level tables for HPC and return code to initialise
initHpc _ (NoHpcInfo {})
  = return ()
initHpc this_mod (HpcInfo tickCount _hashNo)
  = do dflags <- getDynFlags
       when (gopt Opt_Hpc dflags) $
           do emitDataLits (mkHpcTicksLabel this_mod)
                           [ (CmmInt 0 W64)
                           | _ <- take tickCount [0 :: Int ..]
                           ]

