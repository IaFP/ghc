{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE PartialTypeConstructors, TypeOperators, TypeFamilies #-}
{-# OPTIONS -fno-enable-rewrite-rules #-}
#endif
module DsBinds where
import DsMonad     ( DsM )
import CoreSyn     ( CoreExpr )
import TcEvidence (HsWrapper)

dsHsWrapper :: HsWrapper -> DsM (CoreExpr -> CoreExpr)
