{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 903
{-# LANGUAGE QuantifiedConstraints, ExplicitNamespaces, TypeOperators #-}
#endif

-- (those who have too heavy dependencies for GHC.Tc.Types.Evidence)
module GHC.Tc.Types.EvTerm
    ( evDelayedError, evCallStack )
where

import GHC.Prelude

import GHC.Driver.Session

import GHC.Tc.Types.Evidence

import GHC.Unit

import GHC.Builtin.Names
import GHC.Builtin.Types ( unitTy )

import GHC.Core.Type
import GHC.Core
import GHC.Core.Make
import GHC.Core.Utils

import GHC.Types.SrcLoc
import GHC.Types.TyThing
#if MIN_VERSION_base(4,16,0)
import GHC.Types (Total)
#endif

-- Used with Opt_DeferTypeErrors
-- See Note [Deferring coercion errors to runtime]
-- in GHC.Tc.Solver
evDelayedError :: Type -> String -> EvTerm
evDelayedError ty msg
  = EvExpr $
    let fail_expr = mkRuntimeErrorApp tYPE_ERROR_ID unitTy msg
    in mkWildCase fail_expr (unrestricted unitTy) ty []
       -- See Note [Incompleteness and linearity] in GHC.HsToCore.Utils
       -- c.f. mkErrorAppDs in GHC.HsToCore.Utils

-- Dictionary for CallStack implicit parameters
evCallStack :: (
#if MIN_VERSION_base(4,16,0)
  Total m, 
#endif
  MonadThings m, HasModule m, HasDynFlags m) =>
    EvCallStack -> m EvExpr
-- See Note [Overview of implicit CallStacks] in GHC.Tc.Types.Evidence
evCallStack cs = do
  df            <- getDynFlags
  let platform = targetPlatform df
  m             <- getModule
  srcLocDataCon <- lookupDataCon srcLocDataConName
  let mkSrcLoc l = mkCoreConApps srcLocDataCon <$>
               sequence [ mkStringExprFS (unitFS $ moduleUnit m)
                        , mkStringExprFS (moduleNameFS $ moduleName m)
                        , mkStringExprFS (srcSpanFile l)
                        , return $ mkIntExprInt platform (srcSpanStartLine l)
                        , return $ mkIntExprInt platform (srcSpanStartCol l)
                        , return $ mkIntExprInt platform (srcSpanEndLine l)
                        , return $ mkIntExprInt platform (srcSpanEndCol l)
                        ]

  emptyCS <- Var <$> lookupId emptyCallStackName

  pushCSVar <- lookupId pushCallStackName
  let pushCS name loc rest =
        mkCoreApps (Var pushCSVar) [mkCoreTup [name, loc], rest]

  let mkPush name loc tm = do
        nameExpr <- mkStringExprFS name
        locExpr <- mkSrcLoc loc
        -- at this point tm :: IP sym CallStack
        -- but we need the actual CallStack to pass to pushCS,
        -- so we use unwrapIP to strip the dictionary wrapper
        -- See Note [Overview of implicit CallStacks]
        let ip_co = unwrapIP (exprType tm)
        return (pushCS nameExpr locExpr (Cast tm ip_co))

  case cs of
    EvCsPushCall fs loc tm -> mkPush fs loc tm
    EvCsEmpty              -> return emptyCS
