{-
(c) The University of Iowa 2022

-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module GHC.Tc.Deriv.WF ( mk_atat_fam, mk_atat_fam_except
                       , mk_atat_fam_units, mk_atat_fam_except_units
                       -- , elabTyCons
                       , saneTyConForElab
                       , genWFMirrorTyCons, genWFMirrorTyCon, replaceResultWithConstraint
                       , genWFTyFamInst, genWFTyFamInsts
                       ) where


import GHC.Prelude

import GHC.Hs

import GHC.Tc.Utils.Monad
import GHC.Tc.Instance.Family
import GHC.Tc.Utils.Env
import GHC.Tc.Utils.TcMType (mk_wf_name)
import GHC.Core.FamInstEnv

import GHC.Core.TyCo.Rep

import GHC.Core.Type
import GHC.Core.DataCon
import GHC.Types.Name
import GHC.Core.TyCon
import GHC.Core.TyWF
import GHC.Builtin.Types (wfTyConName, wfTyCon, cTupleTyConName, constraintKind)
import GHC.Types.SrcLoc
import GHC.Utils.Outputable as Outputable

import Data.Maybe (fromJust)


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
                -- ; sizeof mpred <= sizeof mkTyConApp (mkTyConApp tc tyd) ty
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

{-                               
-- mk_atat_datacon :: SrcSpan -> TyCon -> DataCon -> TcM ThetaType
-- mk_atat_datacon loc tycon dc = mk_atat_datacon_except loc tycon [] dc
-- Consider
--        n  n   r                     r           n
-- data T k1 k2 (f :: k2 -> Type) (g :: k1 -> k2) (p :: k1) where
--      MkT1 :: f -> p -> T f g p
--      MkT2 :: g -> p -> T f g p
-- We don't want (f @ g p) to creap in hence this bad_bndr dance, as k1 and k2 are specified binders
mk_atat_datacon_except :: TyCon -> [TyCon] -> DataCon -> TcM ThetaType
mk_atat_datacon_except tycon skip_tcs dc =
  do { let arg_tys' = fmap scaledThing $ dataConOrigArgTys dc
           -- univ_roles = zip (dataConUnivTyVars dc) (tyConRoles tycon)
           -- bad_tys = map (TyVarTy . fst) (filter (\(_, r) -> r == Nominal) univ_roles)
           arg_tys = filter (isGoodTyArg tycon) arg_tys'
     -- ; traceTc "mk_atat_datacon"
     --   (vcat [ text "dc=" <> ppr dc
     --         , text "tycon=" <> ppr tycon <+> ppr (tyConArity tycon) <> ppr loc
     --         , text "tycon bndrs=" <> ppr (tyConBinders tycon)
     --         , text "arg_tys=" <> (ppr arg_tys)
     --         -- , text "bad_tys" <> ppr bad_tys
     --         , text "univTys=" <> ppr univ_roles] )
     ; elabty_arg_atats <- mapM (genAtAtConstraintsExceptTcM True (tycon:skip_tcs) []) arg_tys
     ; let arg_atats = fmap newPreds elabty_arg_atats
     ; let atats = foldl mergeAtAtConstraints [] arg_atats
     -- ; traceTc "elab atats=" (ppr atats)
     ; return atats
     }
  where
    isGoodTyArg :: TyCon -> Type -> Bool
    isGoodTyArg tc (TyConApp tyc _) = not (tc == tyc)
    isGoodTyArg _ _ = True
-}

mk_atat_fam :: SrcSpan -> TyCon -> TcM [FamInst]
mk_atat_fam loc tc = mk_atat_fam_except loc tc []

-- | just like mk_atat_fam but generates T @ a ~ () for all possible axioms generatable
mk_atat_fam_units :: SrcSpan -> TyCon -> TcM [FamInst]
mk_atat_fam_units loc tc = mk_atat_fam_except_units loc tc []

mk_atat_fam_except :: SrcSpan -> TyCon -> [TyCon] -> TcM [FamInst]
mk_atat_fam_except loc tc skip_tcs
  | isClassTyCon tc
  = return []
  | (isAlgTyCon tc && saneTyConForElab tc) -- is this a vanilla tycon
    || isNewTyCon tc 
  = do { elabds <- mapM (genAtAtConstraintsExceptTcM False (tc:skip_tcs) []) dt_ctx
       ; let css = fmap newPreds elabds
             elab_dt_ctx = foldl mergeAtAtConstraints [] css
       ; mk_atat_fam' loc [] tc univTys ([], tyargs) ([], tyvars_binder_type) (mergeAtAtConstraints elab_dt_ctx dt_ctx) }
  | isDataFamilyTyCon tc
  = mk_atat_fam_units loc tc
  | otherwise
  = return []
  where
    dcs = visibleDataCons $ algTyConRhs tc
    univTys = mkTyVarTys $ concatMap dataConExTyCoVars dcs
    dt_ctx = tyConStupidTheta tc
    tyvars = tyConTyVars tc
    -- roles = tyConRoles tc
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
  -- maybe simplify to isDataTyCon?
  = do mk_atat_fam' loc [] tc univTys ([], tyargs) ([], tyvars_binder_type) []
  | otherwise = return []
  where
    dcs = visibleDataCons $ algTyConRhs tc
    univTys = mkTyVarTys $ concatMap dataConExTyCoVars dcs
    -- dt_ctx = []
    tyvars = tyConTyVars tc
    -- roles = tyConRoles tc
    binders = tyConBinders tc
    tyvar_binder = zip tyvars binders
    tyvars_binder_type = map (\(t, b) ->
                                 (t, (isVisibleTyConBinder b
                                       && not (isNamedTyConBinder b)))
                                       -- && not (r == Nominal)))
                             ) tyvar_binder
    tyargs = mkTyVarTys tyvars



getMatchingPredicates :: Type     -- Has to exists
                      -> [Type]   -- Should not exist
                      -> [PredType]
                      -> TcM Type
getMatchingPredicates t tvs preds =
    do { preds <- concatMapM flatten_atat_constraint mpreds
       ; let n = length preds
       ; if n == 1 then return (head preds)
         else do { ctupleTyCon <- tcLookupTyCon (cTupleTyConName n)
                 ; return $ mkTyConApp ctupleTyCon preds
                 }
       }
  where
    mpreds = getMatchingPredicates' t tvs preds

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


genWFMirrorTyCons :: [TyCon] -> TcM [(TyCon, TyCon)]
genWFMirrorTyCons = mapM genWFMirrorTyCon

genWFMirrorTyCon :: TyCon -> TcM (TyCon, TyCon)
genWFMirrorTyCon tc
  | isOpenFamilyTyCon tc
  = do { wf_tc_name <- mk_wf_name $ tyConName tc
       ; (mirror_tc, n_tc) <- fixM $
         (\_ -> do { let mirror_tc = mkWFMirrorTyCon
                                     wf_tc_name
                                     constraintKind
                                     -- (replaceResultWithConstraint $ tyConResKind tc)
                                     n_tc
                         n_tc      = updateWfMirrorTyCon tc $ Just mirror_tc
                   ; return (mirror_tc, n_tc)
                   }
         )
       ; traceTc "wf tf mirror open occname:" (ppr wf_tc_name)
       ; return $ (mirror_tc, n_tc)
       }
  | otherwise
  = do { traceTc "wf tf mirror unknown case:" (ppr (famTyConFlav_maybe tc) <+> ppr tc)
       ; return (tc, tc)
       }



-- given a type family instance equation -
-- D a b ~ T a b
-- generates a WF_D a equation
-- WF_D a b ~ wf(T a b)
-- WF_D a b ~ (T @ a, T a @ b)
genWFTyFamInst :: FamInst -> TcM FamInst
genWFTyFamInst fam_inst
  = do { let (tfTc, ts) = famInstSplitLHS fam_inst
             rhs = famInstRHS fam_inst
       ; wfTc_mb <- lookupWfMirrorTyCon tfTc
       ; let wfTc = fromJust wfTc_mb
             loc = noAnnSrcSpan . getSrcSpan $ fam_inst
       ; inst_name <- newFamInstTyConName (L loc (getName wfTc)) ts
       ; elabDetails <- genAtAtConstraintsTcM False rhs
       ; let preds = newPreds elabDetails
             n = length preds
       ; rhs_ty <- if n == 1 then return . head $ preds
                   else do { ctupleTyCon <- tcLookupTyCon (cTupleTyConName n)
                           ; return $ mkTyConApp ctupleTyCon preds
                           }
       ; let tvs     = fi_tvs fam_inst
             lhs_tys = ts
             axiom = mkSingleCoAxiom Nominal inst_name tvs [] [] wfTc lhs_tys rhs_ty
       ; traceTc "elabWfFamInst buildingAxiom: " (vcat [ parens (ppr inst_name)
                                                       , ppr wfTc <+> ppr lhs_tys <+> text "~" <+> ppr rhs_ty
                                                       ])
       ; newFamInst SynFamilyInst axiom
       }


genWFTyFamInsts :: [FamInst] -> TcM [FamInst]
genWFTyFamInsts = mapM genWFTyFamInst
