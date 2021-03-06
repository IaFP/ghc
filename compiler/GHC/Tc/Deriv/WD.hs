{-
(c) The University of Iowa 2022

-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module GHC.Tc.Deriv.WD ( mk_atat_fam, mk_atat_fam_except
                       , mk_atat_fam_units, mk_atat_fam_except_units
                       , genWDTyFamInst, genWDTyFamInsts
                       , genWDFamInstConstraint
                       , mk_datafam_wds
                       ) where


import GHC.Prelude

import GHC.Hs

import GHC.Tc.Utils.Monad
import GHC.Tc.Instance.Family
import GHC.Tc.Utils.Env
import GHC.Tc.Utils.TcType
import GHC.Core.TyCo.Rep

import GHC.Core.Type
import GHC.Core.FamInstEnv
import GHC.Core.Coercion.Axiom
import GHC.Core.DataCon
import GHC.Types.Name
import GHC.Core.TyCon
import GHC.Core.TyWD
import GHC.Types.Var.Set
import GHC.Builtin.Types (wdTyConName, wdTyCon, cTupleTyCon)
import GHC.Types.SrcLoc
import GHC.Utils.Outputable as Outputable

import Data.List (singleton)


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

mk_atat_fam :: SrcSpan -> TyCon -> TcM [FamInst]
mk_atat_fam loc tc = mk_atat_fam_except loc tc []

-- | just like mk_atat_fam but generates T @ a ~ () for all possible axioms that can be generated
mk_atat_fam_units :: SrcSpan -> TyCon -> TcM [FamInst]
mk_atat_fam_units loc tc = mk_atat_fam_except_units loc tc []

mk_atat_fam_except :: SrcSpan -> TyCon -> [TyCon] -> TcM [FamInst]
mk_atat_fam_except loc tc skip_tcs
  | isClassTyCon tc || isWDMirrorTyCon tc
  = return []
  | isDataFamilyTyCon tc -- we never hit this branch
  = mk_atat_fam_units loc tc
  | (isAlgTyCon tc && saneTyConForElab tc) -- is this a vanilla tycon
  || isNewTyCon tc
  = do { elabds <- mapM (genAtAtConstraintsExceptTcM True (tc:skip_tcs) []) dt_ctx
       -- This elaboration is necessary as we may have a type syn that needs expansion in the datatype context
       ; let css = fmap newPreds elabds
             elab_dt_ctx = foldl mergeAtAtConstraints [] css
             css' =  mergeAtAtConstraints elab_dt_ctx dt_ctx
       ; mk_atat_fam' loc [] tc univTys ([], tyargs) ([], tyvars_binder_type) css'
       }
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
  = do mpreds <- redConstraints True $ getMatchingPredicates' t tvs preds
       let n = length mpreds
       if n == 1
         then return $ head mpreds
         else return $ mkTyConApp (cTupleTyCon n) mpreds

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

-- Given the RHS of a fam instance, e.g, "Tree a" in
--   F [a] = Tree a
-- return the corresponding constraint(s), e.g,
--   Tree @ a
genWDFamInstConstraint :: Type -> TcM Type
genWDFamInstConstraint rhs
  = do { preds <- genWdConstraintsTcM False rhs []
       ; let n = length preds
       ; if n == 1
         then return $ head preds
         else return $ mkTyConApp (cTupleTyCon n) preds
       }

-- This is called when we are type checking a data family instance
mk_datafam_wds :: SrcSpan -> TyCon -> [Type] -> TyCon -> TcM [FamInst]
mk_datafam_wds loc fam_tc pats rep_tc
  | isDataFamilyTyCon fam_tc
  , let bndrs_and_pats = zip (tyConBinders fam_tc) pats
        d_ctxt = tyConStupidTheta rep_tc
  = do traceTc "wdelab mk_datafam_wds" (ppr $ map (\(b, p) -> (b, p, tcTypeKind p)) bndrs_and_pats)
       mk_datafam_wds' loc fam_tc bndrs_and_pats [] (tyConTyVars fam_tc) [] d_ctxt []
  | otherwise
  = return []

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
       ; inst_name <- newFamInstTyConName (L (noAnnSrcSpan loc) wdTyConName) ((mkTyConTy tc):tyd')
       ; let argK = tcTypeKind ty
             f = mkTyConApp tc tyd
             fk = tcTypeKind f
             resK = piResultTy fk argK
             lhs_tys = [argK, resK, f, ty]
             axiom_vars = dVarSetElems . mkDVarSet $ concatMap predTyVars lhs_tys
             axiom = mkSingleCoAxiom Nominal inst_name axiom_vars [] [] wdTyCon  lhs_tys mpred
       ; traceTc "mk_atat_fam building axiom " (vcat [ ppr inst_name, ppr axiom_vars
                                                     , parens (ppr f <> dcolon <> ppr fk)
                                                       <+> ppr wdTyCon
                                                       <+> parens (ppr ty <> dcolon <> ppr argK)
                                                       <+> text "~"
                                                       <+> ppr mpred
                                                     , text "isForallTy: " <> ppr (isForAllTy argK)])

       ; fam <- newFamInst SynFamilyInst axiom
       ; mk_atat_fam' loc (fam:acc) tc uTys (tyd', tyl) (tyvarsd', tyvarsl) ctxt }
    | otherwise
    =  mk_atat_fam' loc acc tc uTys (tyd', tyl) (tyvarsd', tyvarsl) ctxt
  where
    tyd' = tyd ++ [ty]
    tyvarsd' = dVarSetElems . mkDVarSet $ tyvarsd ++ [tyvar]
mk_atat_fam' _ acc _ _ _ _ _ = return acc


mk_datafam_wds' :: SrcSpan -> TyCon
                -> [(TyConBinder,Type)] -> [Type]
                -> [TyVar] -> [TyVar]
                -> [PredType]
                -> [FamInst] -> TcM [FamInst]
mk_datafam_wds' _ _ [] _ _ _ _ acc = return acc
mk_datafam_wds' _ _ _ _ [] _ _ acc = return acc
mk_datafam_wds' loc fam_tc ((bndr,ty_arg):ty_rest) tyds (tyvar:tyvars_rest) tyvards theta acc
  | AnonTCB VisArg <- binderArgFlag bndr
  = do let tyd' = tyds ++ [ty_arg]
           tyvarsd' = if isTyVarTy ty_arg
             then tyvards ++ [getTyVar "mk_datafam_wds" ty_arg]
             else tyvards ++ [tyvar]
       inst_name <- newFamInstTyConName (L (noAnnSrcSpan loc) wdTyConName) ((mkTyConTy fam_tc):tyd')
       let argK = tcTypeKind ty_arg
           f = mkTyConApp fam_tc tyds
           fk = tcTypeKind f
           resK = piResultTy fk argK
           rhs_pred = mkTyConTy (cTupleTyCon 0)
           lhs_tys = [argK, resK, f, ty_arg]
           axiom_vars = dVarSetElems . mkDVarSet $ concatMap predTyVars lhs_tys
           axiom = mkSingleCoAxiom Nominal inst_name axiom_vars [] [] wdTyCon lhs_tys rhs_pred
       traceTc "wdelab datafam axiom" (vcat [ parens (ppr inst_name)
                                            , parens (ppr f <> dcolon <> ppr fk)
                                                       <+> ppr wdTyCon
                                                       <+> parens (ppr ty_arg <> dcolon <> ppr argK)
                                                       <+> text "~"
                                                       <+> ppr rhs_pred
                                            , ppr axiom_vars <+> ppr wdTyCon <+> ppr lhs_tys <+> text "~" <+> ppr rhs_pred
                                            , text "isForallTy: " <> ppr (isForAllTy argK)
                                            ])
       fam <- newFamInst SynFamilyInst axiom
       mk_datafam_wds' loc fam_tc ty_rest tyd' tyvars_rest tyvarsd' theta (fam:acc)
  | otherwise
  , let tyd' = tyds ++ [ty_arg]
        tyvarsd' = if isTyVarTy ty_arg
                   then tyvards ++ [getTyVar "mk_datafam_wds" ty_arg]
                   else tyvards ++ [tyvar]
  = mk_datafam_wds' loc fam_tc ty_rest tyd' tyvars_rest tyvarsd' theta acc


{-
Consider a data family and an instance
data family DF (a::k)
data instance DF (a::Bool) where
    STrue  :: DF 'True
    SFalse :: DF 'False

-}

-- given a type family instance equation
-- D a b ~ T a b
-- generates a WD_D a equation
-- $wd:D a b ~ wdt(T a b)
-- or $wd: a b ~ (T @ a, T a @ b)
-- and even simplify it to $wd:D a b ~ () if T is total
genWDTyFamInst :: FamInst -> TcM [FamInst] -- can be [] if there's a Sing
genWDTyFamInst fam_inst
  | SynFamilyInst <- famInstFlavor fam_inst
  = do { let (fam_tc, ts) = famInstSplitLHS fam_inst
             rhs_ty = famInstRHS fam_inst
       ; let wd_tc = wdMirrorTyCon "genWDTyFamInst" fam_tc
             loc = noAnnSrcSpan . getSrcSpan $ fam_inst
       ; inst_name <- newFamInstTyConName (L loc (getName wd_tc)) ts
       ; wd_rhs_ty <- genWDFamInstConstraint rhs_ty
       ; let tvs     = fi_tvs fam_inst
             lhs_tys = ts
             axiom = mkSingleCoAxiom Nominal inst_name tvs [] [] wd_tc lhs_tys wd_rhs_ty
       ; traceTc "wdelab axiom synfam" (vcat [ parens (ppr inst_name)
                                              , ppr wd_tc <+> ppr lhs_tys <+> text "~" <+> ppr wd_rhs_ty
                                              ])
       ; singleton <$> newFamInst SynFamilyInst axiom
       }
  | otherwise -- data families instance equations are eta reducible as they are representational 
  = return []
  
genWDTyFamInsts :: [FamInst] -> TcM [FamInst]
genWDTyFamInsts = concatMapM genWDTyFamInst



