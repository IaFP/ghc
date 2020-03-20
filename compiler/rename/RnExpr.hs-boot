{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE PartialTypeConstructors, TypeOperators, TypeFamilies #-}
{-# OPTIONS -fno-enable-rewrite-rules #-}
#endif
module RnExpr where
import Name
import GHC.Hs
import NameSet     ( FreeVars )
import TcRnTypes
import SrcLoc      ( Located )
import Outputable  ( Outputable )

rnLExpr :: LHsExpr GhcPs
        -> RnM (LHsExpr GhcRn, FreeVars)

rnStmts :: --forall thing body.
           Outputable (body GhcPs) => HsStmtContext Name
        -> (Located (body GhcPs) -> RnM (Located (body GhcRn), FreeVars))
        -> [LStmt GhcPs (Located (body GhcPs))]
        -> ([Name] -> RnM (thing, FreeVars))
        -> RnM (([LStmt GhcRn (Located (body GhcRn))], thing), FreeVars)
