{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 810
#endif

module GHC.Platform.NoRegs where

import GhcPrelude

#define MACHREGS_NO_REGS 1
#include "../../../includes/CodeGen.Platform.hs"

