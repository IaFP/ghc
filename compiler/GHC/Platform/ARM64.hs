{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 810
#endif

module GHC.Platform.ARM64 where

import GhcPrelude

#define MACHREGS_NO_REGS 0
#define MACHREGS_aarch64 1
#include "../../../includes/CodeGen.Platform.hs"

