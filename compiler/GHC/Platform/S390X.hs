{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 810
#endif

module GHC.Platform.S390X where

import GhcPrelude

#define MACHREGS_NO_REGS 0
#define MACHREGS_s390x 1
#include "../../../includes/CodeGen.Platform.hs"

