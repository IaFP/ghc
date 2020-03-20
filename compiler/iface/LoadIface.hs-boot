
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE PartialTypeConstructors, TypeOperators, TypeFamilies #-}
{-# OPTIONS -fno-enable-rewrite-rules #-}
#endif
module LoadIface where
import Module (Module)
import TcRnMonad (IfM)
import HscTypes (ModIface)
import Outputable (SDoc)

loadSysInterface :: SDoc -> Module -> IfM lcl ModIface
