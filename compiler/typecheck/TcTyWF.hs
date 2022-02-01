{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE PartialTypeConstructors, TypeOperators, TypeFamilies #-}
#endif
module TcTyWF (

  ------------------------------
  -- wellformed constraint generation
  genAtAtConstraints, genAtAtConstraintsTcM, genAtAtConstraintsExcept, genAtAtConstraintsExceptTcM,
  attachConstraints, mergeAtAtConstraints,
  elabAtAtConstraintsTcM, elabWithAtAtConstraintsTopTcM,-- unelabAtAtConstraints, unelabAtAtConstraintsM, 
  predTyArgs, predTyVars, flatten_atat_constraint, saneTyConForElab 
  ) where

import FamInst (tcGetFamInstEnvs)
import FamInstEnv (topNormaliseType)
import GHC.Base (mapM)
import GhcPrelude hiding (mapM)
import PrelNames
import THNames
import TcRnTypes
import TcSMonad (matchFamTcM)
import TcType
import TcValidity (tyConArityErr)
import TyCoRep
import TyCon
import Type hiding (attachConstraints)
import TysWiredIn
import VarSet

import TcRnMonad (failWithTc, traceTc)
import Data.List (partition)
import MonadUtils
import Util
import Outputable

#if __GLASGOW_HASKELL__ >= 810
import GHC.Types (type (@@))
#endif

-------------------------------------------------------------------------
{-
%************************************************************************
%*                                                                      *
             Generating Wellformedness (@@) Constraints
*                                                                      *
************************************************************************

-}

-- | the kind * that is used at type level
star :: Type
star = mkTyConApp liftedTypeKindTyCon []

-- | Elaborate the type with well formed constraints
elabAtAtConstraintsTcM :: Type -> TcM Type
elabAtAtConstraintsTcM ty =
  do { let (covarbndrs, ty') = splitForAllVarBndrs ty
     ; (elabTy, c_extra) <- genAtAtConstraintsTcM ty'
     ; c_extra' <- concatMapM flatten_atat_constraint c_extra
     ; return (mkForAllTys covarbndrs $ attachConstraints c_extra' elabTy) }

-- For the type signature
-- theta => tau
-- the elaboration will give
-- (at'ats U theta) => tau
-- We omit the at'ats that are already present in theta
attachConstraints :: ThetaType -> Type -> Type
attachConstraints constraints ty = 
  mkInvisFunTys theta' tau 
  where (theta, tau) = tcSplitPhiTy ty
        theta' = mergeAtAtConstraints theta constraints

-- Generates all the f @@ a constraints
genAtAtConstraintsTcM :: Type ->  TcM (Type, ThetaType)
genAtAtConstraintsTcM ty = genAtAtConstraintsExceptTcM [] [] ty

-- | Elaborate the type with well formed constraints
--   Also collapse the ones that we know are ()'s
elabWithAtAtConstraintsTopTcM :: Type -> TcM Type
elabWithAtAtConstraintsTopTcM ty =
  do { (elabTy, c_extra) <- genAtAtConstraintsTcM ty
     ; wfcs <- concatMapM flatten_atat_constraint c_extra
     ; return $ attachConstraints wfcs elabTy }


-- Generates all the f @@ a constraints
genAtAtConstraints :: (Monad m
#if __GLASGOW_HASKELL__ >= 810
                       , m @@ ThetaType, m @@ [(Type, ThetaType)]
#endif
                      ) => Type ->  m (Type, ThetaType)
genAtAtConstraints ty = genAtAtConstraintsExcept [] [] ty


-- Generates f @@ a constraints unless tycon passed in appears in LHS
genAtAtConstraintsExcept :: (Monad m
#if __GLASGOW_HASKELL__ >= 810
                            , m @@ ThetaType, m @@ [(Type, ThetaType)]
#endif
                            ) => [TyCon] -> [Type] -> Type ->  m (Type, ThetaType)
genAtAtConstraintsExcept tycons ts ty
  -- | isLiftedRuntimeRep ty || isLiftedTypeKind ty = return (ty, []) -- i don't think we need this now.
  -- it  generates (->) @@ a and (a ->) @@ b and recursively generates constraints for a and b
  -- it is a special case of Type constructor
  | (FunTy VisArg ty1 ty2) <- ty = do -- atc <- (funAt ty1 ty2)
      (elab_ty1, atc_ty1) <- (genAtAtConstraintsExcept tycons ts ty1)
      (elab_ty2, atc_ty2) <- (genAtAtConstraintsExcept tycons ts ty2)
      return (FunTy VisArg elab_ty1 elab_ty2, mergeAtAtConstraints atc_ty1 atc_ty2)

    -- for (=>) types ignore the 1st arg as it is a constraint
  | (FunTy InvisArg constraint ty') <- ty  = do
      (elab_ty, atc_ty) <- (genAtAtConstraintsExcept tycons ts ty')
      return (FunTy InvisArg constraint elab_ty, atc_ty)

  -- recursively build @@ constraints for type constructor
  | (TyConApp tyc tycargs) <- ty = do
      { elabTys_and_atats <- mapM (genAtAtConstraintsExcept (tyc:tycons) ts) tycargs
      ; let (elab_tys, atc_args) = unzip elabTys_and_atats
      ; if any (== tyc) tycons
        then return (TyConApp tyc elab_tys, foldl mergeAtAtConstraints [] atc_args)
        else do
        { atc_tycon <- tyConGenAts tycons ts tyc tycargs
        ; return (TyConApp tyc elab_tys, foldl mergeAtAtConstraints atc_tycon atc_args)
        }
      }
  -- for type application we need ty1 @@ ty2
  -- for type application we need ty1 @@ ty2 (unless ty2 is * then skip it or ty2 has a constraint kind)
  | (AppTy ty1 ty2) <- ty =
      -- if isHigherKinded ty1 -- Don't break * types apart as we don't have a theory for that yet
      -- then return (ty, [])
      -- else
        if (eqType ty2 star)
           || (any (eqType ty2) ts)
           || isTyKindPoly ty1
           || not (null $ fst $ (tyCoFVsOfType ty2)
                    (\v -> any (== v) (map (getTyVar "genAtAtConstraintsExcept") (filter isTyVarTy ts))) emptyVarSet ([], emptyVarSet))
           -- || (isFunTy (tcTypeKind ty1) && eqType (funResultTy (tcTypeKind ty1)) star)
           -- || not (noFreeVarsOfType (tcTypeKind ty1) && noFreeVarsOfType (tcTypeKind ty1))
        then do { (elab_ty1, atc_ty1) <- genAtAtConstraintsExcept tycons ts ty1
                ; return $ (AppTy elab_ty1 ty2, atc_ty1)
                }
        else do { let atc = [ty1 `at'at` ty2]
                ; (elab_ty1, atc_ty1) <- genAtAtConstraintsExcept tycons ts ty1
                ; (elab_ty2, atc_ty2) <- genAtAtConstraintsExcept tycons ts ty2
                ; return $ (AppTy elab_ty1 elab_ty2, mergeAtAtConstraints atc
                                                     $ mergeAtAtConstraints atc_ty1 atc_ty2)
                }
        
  -- recurse inwards
  -- forall k (a :: k). a -> a
  --        |  |
  --        |  -- visible binder (specified)
  --        ----- invisible binder (infered)
  -- forall a b. P a b => ...
  -- forall a b. (P a, Q b) => .. ~ forall a. (P a) => forall b. (Q b) => ... ?
  -- may be it is and lets roll with it.
  -- forall k (f :: k -> Type) (g :: k -> Type) (a :: k). f a -> T f g a
  -- we shouldn't generate f @@ a as (a :: k) or becuase f returns Type.
  | (ForAllTy bndr ty1) <- ty = do
      let bvar = binderVar bndr
          bvarTy = mkTyVarTy bvar
          shouldn'tAtAt = isInvisibleArgFlag (binderArgFlag bndr)
      (elab_ty1, atc_ty1) <- if shouldn'tAtAt
                             then genAtAtConstraintsExcept tycons (bvarTy : ts) ty1
                             else genAtAtConstraintsExcept tycons ts ty1
      let (have'bvar, donthave'bvar) = partition (predHas bvarTy) atc_ty1
          r_ty = ForAllTy bndr (attachConstraints have'bvar elab_ty1)
      return (r_ty, donthave'bvar) -- genAtAtConstraints ty'

  | otherwise = return (ty, [])

  where predHas :: Type -> PredType -> Bool
        predHas tv pred = or [eqType tv x | x <- (predTyArgs pred)] -- no me likey
    

-- Generates f @@ a constraints unless tycon passed in appears in LHS
genAtAtConstraintsExceptTcM :: [TyCon] -> [Type] -- Things to skip 
                            -> Type ->  TcM (Type, ThetaType)
genAtAtConstraintsExceptTcM tycons ts ty
  -- | isLiftedRuntimeRep ty || isLiftedTypeKind ty = return (ty, [])
  -- it  generates (->) @@ a and (a ->) @@ b and recursively generates constraints for a and b
  -- it is a special case of Type constructor
  | (FunTy VisArg ty1 ty2) <- ty = do -- atc <- (funAt ty1 ty2)
      (elab_ty1, atc_ty1) <- (genAtAtConstraintsExceptTcM tycons ts ty1)
      (elab_ty2, atc_ty2) <- (genAtAtConstraintsExceptTcM tycons ts ty2)
      return (FunTy VisArg elab_ty1 elab_ty2, mergeAtAtConstraints atc_ty1 atc_ty2)

    -- for (=>) types ignore the 1st arg as it is a constraint
  | (FunTy InvisArg constraint ty') <- ty  = do
      (elab_ty, atc_ty) <- (genAtAtConstraintsExceptTcM tycons ts ty')
      return (FunTy InvisArg constraint elab_ty, atc_ty)

  -- recursively build @@ constraints for type constructor
  | (TyConApp tyc tycargs) <- ty = do
      { elabTys_and_atats <- mapM (genAtAtConstraintsExceptTcM (tyc:tycons) ts) tycargs
      ; let (elab_tys, atc_args) = unzip elabTys_and_atats
      ; if any (== tyc) tycons
        then return (TyConApp tyc elab_tys, foldl mergeAtAtConstraints [] atc_args)
        else do
        { atc_tycon <- tyConGenAtsTcM tycons ts tyc tycargs
        ; return (TyConApp tyc elab_tys, foldl mergeAtAtConstraints atc_tycon atc_args)
        }
      }
  -- for type application we need ty1 @@ ty2 (unless ty2 is * then skip it or ty2 has a constraint kind)
  | (AppTy ty1 ty2) <- ty =
      -- if isHigherKinded ty1 -- Don't break * types apart as we don't have a theory for that yet
      -- then return (ty, [])
      -- else
        if eqType ty2 star
           || any (eqType ty2) ts
           || isTyKindPoly ty1           
           || not (null $ fst $ (tyCoFVsOfType ty2)
                (\v -> any (== v) (map (getTyVar "genAtAtConstraintsExceptTcM") (filter isTyVarTy ts)))
                             emptyVarSet ([], emptyVarSet))
         -- || (isFunTy (tcTypeKind ty1) && eqType (funResultTy (tcTypeKind ty1)) star)
         -- || not (noFreeVarsOfType (tcTypeKind ty1) && noFreeVarsOfType (tcTypeKind ty1))
        then do { (elab_ty1, atc_ty1) <- genAtAtConstraintsExceptTcM tycons ts ty1
                ; (elab_ty2, atc_ty2) <- genAtAtConstraintsExceptTcM tycons ts ty2
                ; return $ (AppTy elab_ty1 elab_ty2, mergeAtAtConstraints atc_ty1 atc_ty2)
                }
        else do { let atc = [ty1 `at'at` ty2]
                ; (elab_ty1, atc_ty1) <- genAtAtConstraintsExceptTcM tycons ts ty1
                ; (elab_ty2, atc_ty2) <- genAtAtConstraintsExceptTcM tycons ts ty2
                ; return $ (AppTy elab_ty1 elab_ty2, mergeAtAtConstraints atc
                                                   $ mergeAtAtConstraints atc_ty1 atc_ty2)
                }
        
  -- recurse inwards
  -- forall k (a :: k). a -> a
  --        |  |
  --        |  -- visible binder (specified)
  --        ----- invisible binder (infered)
  -- forall a b. P a b => ...
  -- forall a b. (P a, Q b) => .. ~ forall a. (P a) => forall b. (Q b) => ... ?
  -- may be it is and lets roll with it.
  -- forall k (f :: k -> Type) (g :: k -> Type) (a :: k). f a -> T f g a
  -- forall k (a :: k) (f :: k -> *). f a  -> String
  --               vs
  -- forall a b. (a -> b) -> f a -> f b
  -- we shouldn't generate f @@ a as (a :: k)
  | (ForAllTy bndr ty1) <- ty = do
      let bvar = binderVar bndr
          bvarTy = mkTyVarTy bvar
          shouldn'tAtAt = isInvisibleArgFlag (binderArgFlag bndr)
      (elab_ty1, atc_ty1) <- if shouldn'tAtAt
                             then genAtAtConstraintsExceptTcM tycons (bvarTy : ts) ty1
                             else genAtAtConstraintsExceptTcM tycons ts ty1
      let (have'bvar, donthave'bvar) = partition (predHas bvarTy) atc_ty1
          r_ty = ForAllTy bndr (attachConstraints have'bvar elab_ty1)
      return (r_ty, donthave'bvar) -- genAtAtConstraints ty'

  | otherwise = return (ty, [])

  where predHas :: Type -> PredType -> Bool
        predHas tv pred = or [eqType tv x | x <- (predTyArgs pred)] -- no me likey


isTyConInternal :: TyCon -> Bool
isTyConInternal tycon =
  tycon `hasKey` tYPETyConKey || tycon `hasKey` runtimeRepTyConKey
  || tycon `hasKey` repTyConKey || tycon `hasKey` rep1TyConKey
  || tycon `hasKey` typeRepTyConKey || tycon `hasKey` typeableClassKey
  || tycon `hasKey` eqTyConKey || tycon `hasKey` heqTyConKey
  || tycon `hasKey` someTypeRepTyConKey || tycon `hasKey` proxyPrimTyConKey
  || tycon `hasKey` ioTyConKey || (tyConName tycon == ioTyConName)
  || tycon `hasKey` listTyConKey
  || tycon `hasKey` maybeTyConKey
  || isBoxedTupleTyCon tycon || isUnboxedTupleTyCon tycon
  || isUnboxedSumTyCon tycon
  || tycon `hasKey` stablePtrPrimTyConKey || tycon `hasKey` stablePtrTyConKey
  || tycon `hasKey` staticPtrTyConKey || (tyConName tycon == staticPtrTyConName)
  || tycon `hasKey` staticPtrInfoTyConKey || (tyConName tycon == staticPtrInfoTyConName)
  || tycon `hasKey` ptrTyConKey || tycon `hasKey` funPtrTyConKey
  || tycon `hasKey` qTyConKey || tyConName tycon == qTyConName
  || tycon `hasKey` tExpTyConKey
  || tycon == funTyCon

saneTyConForElab :: TyCon -> Bool
saneTyConForElab tycon =
  not (isUnboxedTupleTyCon tycon
       || isDataFamilyTyCon tycon
       || isPrimTyCon tycon
       || isPromotedDataCon tycon
       || isClassTyCon tycon
       || isFunTyCon tycon
       || isFamilyTyCon tycon )

-- recursively generates @@ constraints for a type constructor
-- Also rewrite Type family constructors
tyConGenAtsTcM :: [TyCon]
               -> [Type] --- things to ignore
               -> TyCon -> [Type] -> TcM ThetaType
tyConGenAtsTcM eTycons ts tycon args -- TODO isUnliftedType??
  | isTyConInternal tycon
  = return []
  | isTypeSynonymTyCon tycon = 
      if (args `lengthAtLeast` (tyConArity tycon))
      then case coreView (TyConApp tycon args) of
             Just ty   -> do { (_, cs) <- genAtAtConstraintsExceptTcM eTycons ts ty
                             -- ; traceTc "tysyn tyConGenAtsTcM: " (ppr ty)
                             ; return cs }
             Nothing   -> pprPanic "tysyn tyConGenAts" (ppr tycon)
      else failWithTc (tyConArityErr tycon args)
  | isTyConAssoc tycon -- && not (isNewTyCon tycon)
  = do { let (args', extra_args) = splitAt (tyConArity tycon) (zip args (zip (tyConBinders tycon) (tyConRoles tycon)))
       -- ; traceTc "tyconassoc tyConGensAtsTcM: " (text "TyCon " <> ppr tycon
       --                                           <+> ppr (tyConArity tycon)
       --                                           <+> text "args " <> ppr args'
       --                                           <+> text "extra_args " <> ppr extra_args)
       ; recGenAts' tycon extra_args (map fst args') [] ts
       }
  | isTypeFamilyTyCon tycon
    || isDataFamilyTyCon tycon
  = do {
       ; elabtys_and_css <- mapM (genAtAtConstraintsExceptTcM eTycons ts) args
       ; let (_, css) = unzip elabtys_and_css
       ; co_ty_mb <- matchFamTcM tycon args
       ; traceTc "co_ty_mb: " (ppr co_ty_mb)
       ; case co_ty_mb of
           -- This happens if the type fam application *doesn't* elaborate to anything
           Nothing -> do {
             ; traceTc "type args: " (ppr args)
             ; let initial = case wfConstraintTc tycon of
                               Nothing -> []
                               -- Just wf -> [mkTyConApp wf  $ (map tcTypeKind args) ++ args]
                               Just wf -> [mkTyConApp wf args]
             ; return $ foldl mergeAtAtConstraints initial css
             }
                      
           -- If TF app *does* elaborate we are fine
           Just (_, ty) -> do {
             ; (_, cs) <- genAtAtConstraints ty
             ; return $ foldl mergeAtAtConstraints cs css
             }
       }
  | not (saneTyConForElab tycon)
  = do { elabtys_and_css <- mapM (genAtAtConstraintsExceptTcM (tycon:eTycons) ts) args
       ; let (_, css) = unzip elabtys_and_css
       ; return $ foldl mergeAtAtConstraints [] css
       }
  | otherwise = recGenAts tycon args ts


-- recursively generates @@ constraints for a type constructor
-- Doesn't rewrite type family constructors
tyConGenAts :: (Monad m
#if __GLASGOW_HASKELL__ >= 810
               , m @@ ThetaType, m @@ [(Type, ThetaType)]
               , m @@ (Type, ThetaType)
#endif
               )  => [TyCon]
            -> [Type] -- things to ignore
            -> TyCon -> [Type] -> m ThetaType
tyConGenAts eTycons ts tycon args -- TODO isUnliftedType??
  | isTyConInternal tycon
  = return []
  | isTyConAssoc tycon && not (isNewTyCon tycon)
  = do { let (args', extra_args) = splitAt (length (tyConVisibleTyVars tycon))
                                   (zip args (zip  (tyConBinders tycon) (tyConRoles tycon)) )
       ; recGenAts' tycon extra_args (map fst args') [] ts
       }
  | isTypeSynonymTyCon tycon =
      if (args `lengthAtLeast` (tyConArity tycon))
      then case coreView (TyConApp tycon args) of
             Just ty   -> do { (_, cs) <- genAtAtConstraintsExcept eTycons ts ty
                             -- ; traceTc "tysyn tyConGenAts: " (ppr ty)
                             ; return cs }
             Nothing   -> pprPanic "tysyn tyConGenAts" (ppr tycon)
      else pprPanic "tysyn tyConGenAts" (ppr tycon)
      
      -- case coreView (TyConApp tycon args) of
      --   Just ty   -> do {(_, cs) <- genAtAtConstraintsExcept eTycons ty; return cs}
      --   Nothing   -> pprPanic "tyConGenAts" (ppr tycon)
  | not (saneTyConForElab tycon)
  = do { elabtys_and_css <- mapM (genAtAtConstraintsExcept (tycon:eTycons) ts) args
       ; let (_, css) = unzip elabtys_and_css
       ; return $ foldl mergeAtAtConstraints [] css
       }
  | otherwise = recGenAts tycon args ts


recGenAts :: Monad m => TyCon -> [Type]
          -> [Type] -- things to ignore
          -> m ThetaType
recGenAts tc args ts = recGenAts' tc arg_binder [] [] ts
  where
    binders = tyConBinders tc
    roles = tyConRoles tc
    arg_binder = zip args (zip binders roles)

recGenAts' :: Monad m => TyCon
                      -> [(Type, (TyConBinder, Role))] -- remaning 
                      -> [Type]                -- done
                      -> ThetaType             -- accumuator
                      -> [Type]                -- things to ignore
                      -> m ThetaType
recGenAts' _ [] _ acc _ = return acc
recGenAts' tyc ((hd, (bndr, role)) : tl) tycargs' acc ts
  = do { let atc = if (isNamedTyConBinder bndr) -- TODO: I think there is a cannonical way to do this check.
                      || isInvisibleArgFlag (tyConBinderArgFlag bndr)
                      -- || (role == Nominal) -- don't generate @@'s for nominal rep typeargs as they are non parametric
                      || (any (eqType hd) ts)
                      || (eqType star hd)  -- we don't want f @@ * creaping in
                   then []
                   else [(TyConApp tyc (tycargs')) `at'at` hd]
       ; recGenAts' tyc tl (tycargs' ++ [hd]) (mergeAtAtConstraints acc atc) ts
       }

-- takes in type arguments ty1 ty2 and returns the constraint ty1 @@ ty2
-- we do have to provide the kinds for ty1 and ty2 so
-- that the correctly kinded type is instantiated in @@ class
at'at :: Type -> Type -> PredType
at'at f arg = TyConApp atTyTyCon [argk, resk, f, arg]
  where argk = tcTypeKind arg
        fk   = tcTypeKind f
        resk = piResultTy fk argk

-- function is just a special case of type constructor -> with 2 arguments
-- funAt :: Monad m => Type -> Type -> m ThetaType
-- funAt ty1 ty2 = tyConGenAts (TysPrim.funTyCon) [ty1, ty2]
  

-- Merges two thetas to a theta with unique constraints
mergeAtAtConstraints :: ThetaType -> ThetaType ->  ThetaType
mergeAtAtConstraints = stableMergeTypes

predTyArgs :: PredType -> [Type]
predTyArgs ty
  | (TyVarTy _) <- ty
  = [ty]
  | (AppTy t1 t2) <- ty
  = predTyArgs t1 ++ predTyArgs t2
  | TyConApp _ args <- ty
  = concatMap predTyArgs args
  | (FunTy _ argTy resTy) <- ty
  = predTyArgs argTy ++ predTyArgs resTy
  | otherwise = []

predTyVars :: PredType -> [TyVar]
predTyVars ty
  | (TyVarTy var) <- ty
  = [var]
  | (AppTy t1 t2) <- ty
  = predTyVars t1 ++ predTyVars t2
  | TyConApp _ args <- ty
  = concatMap predTyVars args
  | (FunTy _ argTy resTy) <- ty
  = predTyVars argTy ++ predTyVars resTy
  | otherwise = []

-- | eagerly beat the type to its normal form then if its an empty dictonary return [] else return the actual dictonary
--   the caller of the function doesn't expect any redundant @@'s to be left beind, unless it is a higer order
--   data type. eg. data T f b = MkT f b (f b)
--   MkT :: f @@ b => f -> b -> T f b
--   wf-ctxt (MkT) = f @@ b and we cannot do anything with it unless f is instantiated
flatten_atat_constraint :: Type -> TcM [Type]
flatten_atat_constraint ty@(TyConApp tc _)
  | tc == atTyTyCon || isTypeFamilyTyCon tc
  = do fam_envs <- FamInst.tcGetFamInstEnvs
       let ty' = topNormaliseType fam_envs ty
       tuplesToList ty'
flatten_atat_constraint ty = return [ty]

tuplesToList :: Type -> TcM [Type]
tuplesToList ty
  | (TyConApp tc tork_args) <- ty, isCTupleTyConName $ tyConName tc =
      do return tork_args
  | otherwise = return [ty]
