{-
(c) The University of Iowa 2022

-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module GHC.Tc.Deriv.WF ( mk_atat_fam, mk_atat_fam_except
                       , mk_atat_fam_units, mk_atat_fam_except_units
                       , saneTyConForElab
                       , genWFTyFamInst, genWFTyFamInsts
                       ) where


import GHC.Prelude

import GHC.Hs

import GHC.Tc.Utils.Monad
import GHC.Tc.Instance.Family
import GHC.Tc.Utils.Env
import GHC.Core.FamInstEnv
import GHC.Core.TyCo.Rep

import GHC.Core.Type
import GHC.Core.DataCon
import GHC.Types.Name
import GHC.Core.TyCon
import GHC.Core.TyWF
import GHC.Builtin.Types (wfTyConName, wfTyCon, cTupleTyCon)
import GHC.Types.SrcLoc
import GHC.Utils.Outputable as Outputable



{-
************************************************************************
*                                                                      *
                Generating @ type family instances
*                                                                      *
************************************************************************
Generate @ type family instances for data types
These are always generated for a data type. 

Consider the partial data type

   data (Ord a) => BST a = ...

we want to generate the type family instance

   type instance BST @ a = Ord a

we can easily get hold of the rhs of the type family using the stupid theta.
The lhs of the equation is just a simple singleCoAxiom

if the datatype is complete i.e. the context is empty then we need
to emit a unit type family instance
eg.
    data Complete a = ...

should generate

   type instance Complete @ a = ()

What if the context has more than one constraints?
eg.
   data (C1 a, C2 b) => T a b = ... ?
we generate these type instances:
type famiy T @ a = C a
type family T a @ b = C2 b


We don't bubble out the constraints yet.

We also need to bubble out constraints
from the data constructors consider:
data Ord a => T a b c = forall d e. MkT1 a b e
                      | forall e. MkT2 (D1 c) (D2 e) ...

we would want to bubble these out as:
type instance T a b @ c = D1 @ c

as e is existential in MkT2 we do not bubble out that type instance

-}

mk_atat_fam' :: SrcSpan
            -> [FamInst]
            -> TyCon                       -- TyCon to work on
            -> [Type]                      -- Universals to exclude
            -> ([Type],[Type])             -- Type arguments (done, left)
            -> ([TyVar],[(TyVar, Bool)])   -- Type Variables (done, (left, shouldInclude?))
            -> ThetaType                   -- Datatype context (done, left)
            -> TcM [FamInst]
mk_atat_fam' loc acc tc uTys (tyd, ty:tyl) (tyvarsd, (tyvar, shouldInc):tyvarsl) ctxt
  | shouldInc
  = do { mpred <- getMatchingPredicates ty (tyl ++ uTys) ctxt
       ; inst_name <- newFamInstTyConName (L (noAnnSrcSpan loc) wfTyConName) tyd'
       ; let argK = tcTypeKind ty
             f = mkTyConApp tc tyd
             fk = tcTypeKind f
             resK = piResultTy fk argK
             axiom = mkSingleCoAxiom Nominal inst_name tyvarsd' [] [] wfTyCon
                               [argK, resK, f, ty] mpred
       ; traceTc "building axiom " (vcat [ parens (ppr f <> dcolon <> ppr fk)
                                           <+> ppr wfTyCon
                                           <+> parens (ppr ty <> dcolon <> ppr argK)
                                         , text "isForallTy: " <> ppr (isForAllTy argK)])

       ; fam <- newFamInst SynFamilyInst axiom
       ; mk_atat_fam' loc (fam:acc) tc uTys (tyd', tyl) (tyvarsd', tyvarsl) ctxt }
    | otherwise
    =  mk_atat_fam' loc acc tc uTys (tyd', tyl) (tyvarsd', tyvarsl) ctxt
  where
    tyd' = tyd ++ [ty]
    tyvarsd' = tyvarsd ++ [tyvar]
mk_atat_fam' _ acc _ _ _ _ _ = return acc


mk_atat_fam :: SrcSpan -> TyCon -> TcM [FamInst]
mk_atat_fam loc tc = mk_atat_fam_except loc tc []

-- | just like mk_atat_fam but generates T @ a ~ () for all possible axioms that can be generated
mk_atat_fam_units :: SrcSpan -> TyCon -> TcM [FamInst]
mk_atat_fam_units loc tc = mk_atat_fam_except_units loc tc []

mk_atat_fam_except :: SrcSpan -> TyCon -> [TyCon] -> TcM [FamInst]
mk_atat_fam_except loc tc skip_tcs
  | isClassTyCon tc || isWFMirrorTyCon tc
  = return []
  | (isAlgTyCon tc && saneTyConForElab tc) -- is this a vanilla tycon
    || isNewTyCon tc 
  = do { elabds <- mapM (genAtAtConstraintsExceptTcM True (tc:skip_tcs) []) dt_ctx
       ; let css = fmap newPreds elabds
             elab_dt_ctx = foldl mergeAtAtConstraints [] css
             css' =  mergeAtAtConstraints elab_dt_ctx dt_ctx
       ; mk_atat_fam' loc [] tc univTys ([], tyargs) ([], tyvars_binder_type) css'
       }
  | isDataFamilyTyCon tc
  = mk_atat_fam_units loc tc
  | otherwise
  = return []
  where
    dcs = visibleDataCons $ algTyConRhs tc
    univTys = mkTyVarTys $ concatMap dataConExTyCoVars dcs
    dt_ctx = tyConStupidTheta tc
    tyvars = tyConTyVars tc
    binders = tyConBinders tc
    tyvar_binder = zip tyvars binders
    tyvars_binder_type = map (\(t, b) ->
                                 (t, (isVisibleTyConBinder b
                                       && not (isNamedTyConBinder b)))
                                       -- && not (r == Nominal)))
                             ) tyvar_binder
    tyargs = mkTyVarTys tyvars

-- | TODO: Can optimize and skip on the matching pred funtion call, but meh.
mk_atat_fam_except_units :: SrcSpan -> TyCon -> [TyCon] -> TcM [FamInst]
mk_atat_fam_except_units loc tc _
  | isAlgTyCon tc && saneTyConForElab tc
  -- we don't want class tycons to creep in
  = do mk_atat_fam' loc [] tc univTys ([], tyargs) ([], tyvars_binder_type) []
  | otherwise = return []
  where
    dcs = visibleDataCons $ algTyConRhs tc
    univTys = mkTyVarTys $ concatMap dataConExTyCoVars dcs
    tyvars = tyConTyVars tc
    binders = tyConBinders tc
    tyvar_binder = zip tyvars binders
    tyvars_binder_type = map (\(t, b) -> (t, (isVisibleTyConBinder b && not (isNamedTyConBinder b)))
                             ) tyvar_binder
    tyargs = mkTyVarTys tyvars



getMatchingPredicates :: Type     -- Has to exists
                      -> [Type]   -- Should not exist
                      -> [PredType]
                      -> TcM Type
getMatchingPredicates t tvs preds
  = do let mpreds' = getMatchingPredicates' t tvs preds
       mpreds <- mapM flatten_atat_constraint mpreds'
       let fmpreds = foldl mergeAtAtConstraints [] mpreds
           n = length fmpreds
       if n == 1 then return $ head fmpreds
         else return $ mkTyConApp (cTupleTyCon n) fmpreds

-- filters the appropriate predicates from the given type variables
-- eg:
-- data (C1 a, C2 b, C3 a b, C4 a b c d) => T a b c d = MkT1 a | MkT2 a b | MkT a b c
-- 
-- T @ a       := getMatchingPredicates a [b, c, d] [C1 a, C2 b, C3 a b, C4 a b c d] = [C1 a]
-- T a @ b     := getMatchingPredicates b [c, d] [C1 a, C2 b, C3 a b, C4 a b c d] = [C2 b, C3 a b]
-- T a b @ c   := getMatchingPredicates c [d] [C1 a, C2 b, C3 a b, C4 a b c d] = [ ]
-- T a b c @ d := getMatchingPredicates d [ ] [C1 a, C2 b, C3 a b, C4 a b c d] = [C4 a b c d]
getMatchingPredicates' :: Type     -- Has to exists
                      -> [Type]    -- Should not exist
                      -> [PredType] -> [PredType]
getMatchingPredicates' tv tvs preds =
  filter (\p -> qual tv tvs p) preds
  where qual tv tvs p =
          any (eqType tv) (predTyArgs p)
          && not (or [eqType tv' t | tv' <- tvs, t <- predTyArgs p])

-- given a type family instance equation
-- D a b ~ T a b
-- generates a WF_D a equation
-- WF_D a b ~ wf(T a b)
-- WF_D a b ~ (T @ a, T a @ b)
genWFTyFamInst :: FamInst -> TcM FamInst
genWFTyFamInst fam_inst
  = do { let (tfTc, ts) = famInstSplitLHS fam_inst
             rhs = famInstRHS fam_inst
       ; let wfTc = wfMirrorTyCon tfTc
             loc = noAnnSrcSpan . getSrcSpan $ fam_inst
       ; inst_name <- newFamInstTyConName (L loc (getName wfTc)) ts
       ; elabDetails <- genAtAtConstraintsTcM True rhs
       ; preds' <- mapM flatten_atat_constraint $ newPreds elabDetails
       ; let preds = foldl mergeAtAtConstraints [] preds'
             n = length preds
             rhs_ty = if n == 1
                      then head preds
                      else mkTyConApp (cTupleTyCon n) preds
       ; let tvs     = fi_tvs fam_inst
             lhs_tys = ts
             axiom = mkSingleCoAxiom Nominal inst_name tvs [] [] wfTc lhs_tys rhs_ty
       ; traceTc "wfelab buildingAxiom: " (vcat [ parens (ppr inst_name)
                                                       , ppr wfTc <+> ppr lhs_tys <+> text "~" <+> ppr rhs_ty
                                                       ])
       ; newFamInst SynFamilyInst axiom
       }


genWFTyFamInsts :: [FamInst] -> TcM [FamInst]
genWFTyFamInsts = mapM genWFTyFamInst
