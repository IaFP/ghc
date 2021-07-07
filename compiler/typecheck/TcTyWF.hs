{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE PartialTypeConstructors, TypeOperators, TypeFamilies #-}
#endif
module TcTyWF (

  ------------------------------
  -- wellformed constraint generation
  genAtAtConstraints, genAtAtConstraintsTcM, genAtAtConstraintsExcept, genAtAtConstraintsExceptTcM,
  attachConstraints, mergeAtAtConstraints,
  elabWithAtAtConstraintsTcM, -- unelabAtAtConstraints, unelabAtAtConstraintsM, 
  getAllPredTyArgs

  ) where

import GhcPrelude hiding (mapM)
import GHC.Base (mapM)
import TcRnTypes
import TyCon
import TyCoRep
import Type
import TcType
import TcValidity (tyConArityErr)
import TysWiredIn
import PrelNames
import THNames
import TcSMonad (matchFamTcM)
import Outputable
import Util
import TcRnMonad (failWithTc, traceTc)
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


-- | Elaborate the type with well formed constraints
elabWithAtAtConstraintsTcM :: Type -> TcM Type
elabWithAtAtConstraintsTcM ty =
  do { (elabTy, c_extra) <- genAtAtConstraintsTcM ty
     ; return $ attachConstraints c_extra elabTy }

-- For the type signature
-- forall tvs. theta => tau
-- the elaboration will give
-- forall tvs (theta, at'ats) => tau
attachConstraints :: ThetaType -> Type -> Type
attachConstraints constraints ty = 
  mkSpecForAllTys tvs $ mkInvisFunTys  theta' tau 
  where (tvs, theta, tau) = tcSplitSigmaTy ty
        theta' = mergeAtAtConstraints theta constraints

-- Generates all the f @@ a constraints
genAtAtConstraintsTcM :: Type ->  TcM (Type, ThetaType)
genAtAtConstraintsTcM ty = genAtAtConstraintsExceptTcM [] ty

-- Generates all the f @@ a constraints
genAtAtConstraints :: (Monad m
#if __GLASGOW_HASKELL__ >= 810
                       , m @@ ThetaType, m @@ [(Type, ThetaType)]
#endif
                      ) => Type ->  m (Type, ThetaType)
genAtAtConstraints ty = genAtAtConstraintsExcept [] ty


-- Generates f @@ a constraints unless tycon passed in appears in LHS
genAtAtConstraintsExcept :: (Monad m
#if __GLASGOW_HASKELL__ >= 810
                            , m @@ ThetaType, m @@ [(Type, ThetaType)]
#endif
                            ) => [TyCon] -> Type ->  m (Type, ThetaType)
genAtAtConstraintsExcept tycons ty
  | isLiftedRuntimeRep ty || isLiftedTypeKind ty = return (ty, [])
  -- it  generates (->) @@ a and (a ->) @@ b and recursively generates constraints for a and b
  -- it is a special case of Type constructor
  | (FunTy VisArg ty1 ty2) <- ty = do -- atc <- (funAt ty1 ty2)
      (elab_ty1, atc_ty1) <- (genAtAtConstraintsExcept tycons ty1)
      (elab_ty2, atc_ty2) <- (genAtAtConstraintsExcept tycons ty2)
      return (FunTy VisArg elab_ty1 elab_ty2, mergeAtAtConstraints atc_ty1 atc_ty2)

    -- for (=>) types ignore the 1st arg as it is a constraint
  | (FunTy InvisArg constraint ty') <- ty  = do
      (elab_ty, atc_ty) <- (genAtAtConstraintsExcept tycons ty')
      return (FunTy InvisArg constraint elab_ty, atc_ty)

  -- recursively build @@ constraints for type constructor
  | (TyConApp tyc tycargs) <- ty = do
      { elabTys_and_atats <- mapM (genAtAtConstraintsExcept (tyc:tycons)) tycargs
      ; let (elab_tys, atc_args) = unzip elabTys_and_atats
      ; if any (== tyc) tycons
        then return (TyConApp tyc elab_tys, foldl mergeAtAtConstraints [] atc_args)
        else do
        { atc_tycon <- tyConGenAts tycons tyc tycargs
        ; return (TyConApp tyc elab_tys, foldl mergeAtAtConstraints atc_tycon atc_args)
        }
      }
  -- for type application we need ty1 @@ ty2
  | (AppTy ty1 ty2) <- ty = do let atc = [ty1 `at'at` ty2]
                               (elab_ty1, atc_ty1) <- genAtAtConstraintsExcept tycons ty1
                               (elab_ty2, atc_ty2) <- genAtAtConstraintsExcept tycons ty2
                               return $ (AppTy elab_ty1 elab_ty2, mergeAtAtConstraints atc
                                          $ mergeAtAtConstraints atc_ty1 atc_ty2)
        
  -- recurse inwards
  --  (ForAllTy _ ty1) <- ty = do
  --     let (tvs, _) = splitForAllTys ty
  --     (elab_ty1, atc_ty1) <- genAtAtConstraintsExcept tycon ty1
  --     let (_, not_contains_tvs) = partition (predHas tvs) atc_ty1
  --     let r_ty = attachConstraints atc_ty1 elab_ty1
  --     return (r_ty, not_contains_tvs) -- genAtAtConstraints ty'

  | otherwise = return (ty, [])

  -- where predHas :: [TyVar] -> PredType -> Bool
  --       predHas tvs pred = or [x == y | x <- (getAllPredTyVarArgs pred),  y <- tvs]




-- Generates f @@ a constraints unless tycon passed in appears in LHS
genAtAtConstraintsExceptTcM :: [TyCon] -> Type ->  TcM (Type, ThetaType)
genAtAtConstraintsExceptTcM tycons ty
  | isLiftedRuntimeRep ty || isLiftedTypeKind ty = return (ty, [])
  -- it  generates (->) @@ a and (a ->) @@ b and recursively generates constraints for a and b
  -- it is a special case of Type constructor
  | (FunTy VisArg ty1 ty2) <- ty = do -- atc <- (funAt ty1 ty2)
      (elab_ty1, atc_ty1) <- (genAtAtConstraintsExceptTcM tycons ty1)
      (elab_ty2, atc_ty2) <- (genAtAtConstraintsExceptTcM tycons ty2)
      return (FunTy VisArg elab_ty1 elab_ty2, mergeAtAtConstraints atc_ty1 atc_ty2)

    -- for (=>) types ignore the 1st arg as it is a constraint
  | (FunTy InvisArg constraint ty') <- ty  = do
      (elab_ty, atc_ty) <- (genAtAtConstraintsExceptTcM tycons ty')
      return (FunTy InvisArg constraint elab_ty, atc_ty)

  -- recursively build @@ constraints for type constructor
  | (TyConApp tyc tycargs) <- ty = do
      { elabTys_and_atats <- mapM (genAtAtConstraintsExceptTcM (tyc:tycons)) tycargs
      ; let (elab_tys, atc_args) = unzip elabTys_and_atats
      ; if any (== tyc) tycons
        then return (TyConApp tyc elab_tys, foldl mergeAtAtConstraints [] atc_args)
        else do
        { atc_tycon <- tyConGenAtsTcM tycons tyc tycargs
        ; return (TyConApp tyc elab_tys, foldl mergeAtAtConstraints atc_tycon atc_args)
        }
        }
  -- for type application we need ty1 @@ ty2
  | (AppTy ty1 ty2) <- ty = do let atc = [ty1 `at'at` ty2]
                               (elab_ty1, atc_ty1) <- genAtAtConstraintsExceptTcM tycons ty1
                               (elab_ty2, atc_ty2) <- genAtAtConstraintsExceptTcM tycons ty2
                               return $ (AppTy elab_ty1 elab_ty2, mergeAtAtConstraints atc
                                          $ mergeAtAtConstraints atc_ty1 atc_ty2)
        
  -- recurse inwards
  --  (ForAllTy _ ty1) <- ty = do
  --     let (tvs, _) = splitForAllTys ty
  --     (elab_ty1, atc_ty1) <- genAtAtConstraintsExceptTcM tycon ty1
  --     let (_, not_contains_tvs) = partition (predHas tvs) atc_ty1
  --     let r_ty = attachConstraints atc_ty1 elab_ty1
  --     return (r_ty, not_contains_tvs) -- genAtAtConstraints ty'

  | otherwise = return (ty, [])

  -- where predHas :: [TyVar] -> PredType -> Bool
  --       predHas tvs pred = or [x == y | x <- (getAllPredTyVarArgs pred),  y <- tvs]



-- recursively generates @@ constraints for a type constructor
-- Also rewrite Type family constructors
tyConGenAtsTcM :: [TyCon] -> TyCon -> [Type] -> TcM ThetaType
tyConGenAtsTcM eTycons tycon args -- TODO isUnliftedType??
  | tycon `hasKey` tYPETyConKey || tycon `hasKey` runtimeRepTyConKey
  || tycon `hasKey` repTyConKey || tycon `hasKey` rep1TyConKey
    || tycon `hasKey` typeRepTyConKey || tycon `hasKey` typeableClassKey
    || tycon `hasKey` eqTyConKey || tycon `hasKey` heqTyConKey
    || tycon `hasKey` someTypeRepTyConKey || tycon `hasKey` proxyPrimTyConKey
    || tycon `hasKey` ioTyConKey || (tyConName tycon == ioTyConName)
    || tycon `hasKey` listTyConKey
    || tycon `hasKey` maybeTyConKey || isBoxedTupleTyCon tycon || tycon `hasKey` stablePtrPrimTyConKey
    || tycon `hasKey` stablePtrTyConKey
    || tycon `hasKey` staticPtrTyConKey || (tyConName tycon == staticPtrTyConName)
    || tycon `hasKey` staticPtrInfoTyConKey || (tyConName tycon == staticPtrInfoTyConName)
    || tycon `hasKey` ptrTyConKey || tycon `hasKey` funPtrTyConKey
    || tycon `hasKey` qTyConKey || tyConName tycon == qTyConName
    || tycon `hasKey` tExpTyConKey
  = return []
  | isTyConAssoc tycon && not (isNewTyCon tycon)
  = do { let (args', extra_args) = splitAt (tyConArity tycon) (zip args (tyConBinders tycon))
       ; recGenAtsTcM' tycon extra_args (map fst args') []
       }
  | isTypeSynonymTyCon tycon = 
      if (args `lengthAtLeast` (tyConArity tycon))
      then case tcView (TyConApp tycon args) of
             Just ty   -> do { (_, cs) <- genAtAtConstraintsExceptTcM eTycons ty
                             ; traceTc "tyConGenAtsTcM: " (ppr ty)
                             ; return cs }
             Nothing   -> pprPanic "tyConGenAts" (ppr tycon)
      else failWithTc (tyConArityErr tycon args)
  | isUnboxedTupleTyCon tycon
    || isDataFamilyTyCon tycon
    || isPrimTyCon tycon
    || isPromotedDataCon tycon
    || isClassTyCon tycon
    || isFunTyCon tycon
  = do { elabtys_and_css <- mapM (genAtAtConstraintsExceptTcM (tycon:eTycons)) args
       ; let (_, css) = unzip elabtys_and_css
       ; return $ foldl mergeAtAtConstraints [] css
       }
  | isTypeFamilyTyCon tycon
  = do { elabtys_and_css <- mapM (genAtAtConstraintsExceptTcM eTycons) args
       ; let (_, css) = unzip elabtys_and_css
       ; co_ty_mb <- matchFamTcM tycon args
       ; case co_ty_mb of
           Nothing -> return $ foldl mergeAtAtConstraints [] css
           Just (_, ty) -> do {
             ; (_, cs) <- genAtAtConstraints ty
             ; return $ foldl mergeAtAtConstraints cs css
             }
       }
    
  | otherwise = recGenAtsTcM tycon args
  where
    recGenAtsTcM :: Monad m => TyCon -> [Type] -> m ThetaType
    recGenAtsTcM tc args = recGenAtsTcM' tc arg_binder [] []
      where
        binders = tyConBinders tc
        arg_binder = zip args binders

    recGenAtsTcM' :: Monad m => TyCon
                             -> [(Type, TyConBinder)] -- remaning 
                             -> [Type]                -- done
                             -> ThetaType             -- accumulator
                             -> m ThetaType
    recGenAtsTcM' _ [] _ acc = return acc
    recGenAtsTcM' tyc ((hd, bndr) : tl) tycargs' acc
          = do { let atc = if (not (isNamedTyConBinder bndr)) && isVisibleTyConBinder bndr
                           then [(TyConApp tyc (tycargs')) `at'at` hd]
                           else []
               ; recGenAtsTcM' tyc tl (tycargs' ++ [hd]) (mergeAtAtConstraints acc atc)
               }



-- recursively generates @@ constraints for a type constructor
-- Doesn't rewrite type family constructors
tyConGenAts :: (Monad m
#if __GLASGOW_HASKELL__ >= 810
               , m @@ ThetaType, m @@ [(Type, ThetaType)]
               , m @@ (Type, ThetaType)
#endif
               )  => [TyCon] -> TyCon -> [Type] -> m ThetaType
tyConGenAts eTycons tycon args -- TODO isUnliftedType??
  | tycon `hasKey` tYPETyConKey || tycon `hasKey` runtimeRepTyConKey
  || tycon `hasKey` repTyConKey || tycon `hasKey` rep1TyConKey
    || tycon `hasKey` typeRepTyConKey || tycon `hasKey` typeableClassKey
    || tycon `hasKey` eqTyConKey || tycon `hasKey` heqTyConKey
    || tycon `hasKey` someTypeRepTyConKey || tycon `hasKey` proxyPrimTyConKey
    || tycon `hasKey` ioTyConKey || (tyConName tycon == ioTyConName)
    || tycon `hasKey` listTyConKey
    || tycon `hasKey` maybeTyConKey
    || isBoxedTupleTyCon tycon || tycon `hasKey` stablePtrPrimTyConKey
    || tycon `hasKey` stablePtrTyConKey
    || tycon `hasKey` staticPtrTyConKey || (tyConName tycon == staticPtrTyConName)
    || tycon `hasKey` staticPtrInfoTyConKey || (tyConName tycon == staticPtrInfoTyConName)
    || tycon `hasKey` ptrTyConKey || tycon `hasKey` funPtrTyConKey
    || tycon `hasKey` qTyConKey || tyConName tycon == qTyConName
    || tycon `hasKey` tExpTyConKey
  = return []
  | isTyConAssoc tycon && not (isNewTyCon tycon)
  = do { let (args', extra_args) = splitAt (length (tyConVisibleTyVars tycon)) (zip args (tyConBinders tycon))
       ; recGenAts' tycon extra_args (map fst args') []
       }
  | isTypeSynonymTyCon tycon =
      case coreView (TyConApp tycon args) of
        Just ty   -> do {(_, cs) <- genAtAtConstraintsExcept eTycons ty; return cs}
        Nothing   -> pprPanic "tyConGenAts" (ppr tycon)
  | isUnboxedTupleTyCon tycon
    || isDataFamilyTyCon tycon
    || isPrimTyCon tycon
    || isPromotedDataCon tycon
    || isClassTyCon tycon
    || isFunTyCon tycon
    || isFamilyTyCon tycon
  = do { elabtys_and_css <- mapM (genAtAtConstraintsExcept (tycon:eTycons)) args
       ; let (_, css) = unzip elabtys_and_css
       ; return $ foldl mergeAtAtConstraints [] css
       }
  | otherwise = recGenAts tycon args
  where
    recGenAts :: Monad m => TyCon -> [Type] -> m ThetaType
    recGenAts tc args = recGenAts' tc arg_binder [] []
      where
        binders = tyConBinders tc
        arg_binder = zip args binders

    recGenAts' :: Monad m => TyCon
                             -> [(Type, TyConBinder)] -- remaning 
                             -> [Type]                -- done
                             -> ThetaType             -- accumuator
                             -> m ThetaType
    recGenAts' _ [] _ acc = return acc
    recGenAts' tyc ((hd, bndr) : tl) tycargs' acc
          = do { let atc = if (not (isNamedTyConBinder bndr)) && isVisibleTyConBinder bndr
                           then [(TyConApp tyc (tycargs')) `at'at` hd]
                           else []
               ; recGenAts' tyc tl (tycargs' ++ [hd]) (mergeAtAtConstraints acc atc)
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
mergeAtAtConstraints c1s c2s = matc [] c1s c2s
  where
    matc acc [] []  = acc
    matc acc [] (c2:c2s) =
      if any (eqType c2) acc
      then matc acc [] c2s
      else matc (c2:acc) [] c2s
    matc acc c1s [] = matc acc [] c1s
    matc acc (c1:c1s) c2s =
      if any (eqType c1) acc
      then matc acc c1s c2s
      else if any (eqType c1) c2s
           then matc acc c1s c2s
           else matc (c1:acc) c1s c2s

getAllPredTyArgs :: PredType -> [Type]
getAllPredTyArgs ty
  | (TyVarTy _) <- ty
  = [ty]
  | (AppTy t1 t2) <- ty
  = getAllPredTyArgs t1 ++ getAllPredTyArgs t2
  | TyConApp _ args <- ty
  = concatMap getAllPredTyArgs args
  | (FunTy _ argTy resTy) <- ty
  = getAllPredTyArgs argTy ++ getAllPredTyArgs resTy
  | otherwise = []
