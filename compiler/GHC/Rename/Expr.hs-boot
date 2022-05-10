{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module GHC.Rename.Expr where
import GHC.Types.Name
import GHC.Hs
import GHC.Types.Name.Set ( FreeVars )
import GHC.Tc.Types
import GHC.Utils.Outputable  ( Outputable )
#if MIN_VERSION_base(4,16,0)
import GHC.Types (WDT)
#endif

rnExpr :: HsExpr GhcPs
        -> RnM (HsExpr GhcRn, FreeVars)

rnLExpr :: LHsExpr GhcPs
        -> RnM (LHsExpr GhcRn, FreeVars)

type AnnoBody body
  = ( Outputable (body GhcPs)
    , Anno (StmtLR GhcPs GhcPs (LocatedA (body GhcPs))) ~ SrcSpanAnnA
    , Anno (StmtLR GhcRn GhcPs (LocatedA (body GhcPs))) ~ SrcSpanAnnA
    , Anno (StmtLR GhcRn GhcRn (LocatedA (body GhcRn))) ~ SrcSpanAnnA
    )
rnStmts :: --forall thing body.
           (
#if MIN_VERSION_base(4,16,0)
             WDT (Anno (StmtLR GhcRn GhcPs (LocatedA (body GhcPs))))
           ,
#endif
           AnnoBody body) => HsStmtContext GhcRn
        -> (body GhcPs -> RnM (body GhcRn, FreeVars))
        -> [LStmt GhcPs (LocatedA (body GhcPs))]
        -> ([Name] -> RnM (thing, FreeVars))
        -> RnM (([LStmt GhcRn (LocatedA (body GhcRn))], thing), FreeVars)
