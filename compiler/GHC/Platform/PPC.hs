{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 810
#endif

module GHC.Platform.PPC where

import GhcPrelude

#define MACHREGS_NO_REGS 0
#define MACHREGS_powerpc 1
#include "../../../includes/CodeGen.Platform.hs"

