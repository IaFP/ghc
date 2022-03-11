{-
(c) The University of Iowa 2022

-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 903
{-# LANGUAGE QuantifiedConstraints, ExplicitNamespaces, TypeOperators #-}
#endif
module GHC.Core.TyWF (

  ------------------------------
  -- wellformed constraint generation
  WfElabTypeDetails (..)
  , genWfConstraints,  genAtAtConstraintsTcM
  , genAtAtConstraintsExceptTcM
  , attachConstraints, mergeAtAtConstraints
  , elabAtAtConstraintsTcM, elabWithAtAtConstraintsTopTcM
  , elabAtAtConstraints -- unelabAtAtConstraintsM, 
  , predTyArgs, predTyVars, flatten_atat_constraint
  , saneTyConForElab
  -- , replaceResultWithConstraint -- , lookupWfMirrorTyCon
  ) where

import GHC.Tc.Instance.Family (tcGetFamInstEnvs)
import GHC.Core.FamInstEnv (topNormaliseType)
import GHC.Base (mapM)
import GHC.Prelude hiding (mapM)
import GHC.Tc.Solver.Monad (matchFamTcM)
import GHC.Tc.Utils.TcType
import GHC.Tc.Validity (tyConArityErr)
import GHC.Core.TyCo.Rep
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Core.Reduction (reductionReducedType)
import GHC.Builtin.Names
import GHC.Builtin.Names.TH
import GHC.Builtin.Types (liftedTypeKindTyCon, isCTupleTyConName, wfTyCon)
import Data.Maybe (maybeToList)
import GHC.Tc.Utils.Monad
import GHC.Utils.Panic (pprPanic)
import GHC.Utils.Outputable
import GHC.Utils.Misc(lengthAtLeast)

#if MIN_VERSION_base(4,16,0)
import GHC.Types (Total)
#endif

-------------------------------------------------------------------------
{-
%************************************************************************
%*                                                                      *
             Generating Wellformedness (@@) Constraints
*                                                                      *
************************************************************************

-}
-- [Note isTyConPhase]
-- This is to mark that we are elaborating the type constructors.
-- setting isTyConPhase to false means we are elaborating a type signature


-- | Data that represents the return type after elaboration
--   TODO: I don't know how to represent this for RankNTypes
data WfElabTypeDetails = WfTyElabDetails { elabTy :: Type        -- The type after elaboration
                                         , newPreds :: ThetaType -- the newly added constraints
                                         }

elabDetails :: Type -> ThetaType -> WfElabTypeDetails
elabDetails ty theta = WfTyElabDetails { elabTy = ty
                                       , newPreds = theta}

instance Outputable WfElabTypeDetails where
  ppr e = vcat [ text "elabTy=" <> ppr (elabTy e)
               , text "elabPreds=" <> ppr (newPreds e)]

-- | the kind * that is used at type level
star :: Type
star = mkTyConApp liftedTypeKindTyCon []

-- | Elaborate the type with well formed constraints
elabAtAtConstraintsTcM :: Bool -> Type -> TcM Type
elabAtAtConstraintsTcM isTyConPhase ty
  | isForAllTy ty = do
      { let (covarbndrs, ty') = splitForAllTyCoVarBinders ty
      ; elabd <- genAtAtConstraintsTcM isTyConPhase ty'
      ; c_extra' <- concatMapM flatten_atat_constraint (newPreds elabd)
      ; let eTy = mkForAllTys covarbndrs $ attachConstraints c_extra' (elabTy elabd)
      ; traceTc "elabWfType(foralled)=" (vcat [ text "tyconphase" <+> ppr isTyConPhase
                                              , text "before:" <+> ppr ty
                                              , text "after:" <+> ppr eTy])
      ; return eTy
      }
  | otherwise = do
      { eTy <- elabWithAtAtConstraintsTopTcM isTyConPhase ty
      ; traceTc "elabWfType(vanilla)=" (vcat [ text "tyconphase" <+> ppr isTyConPhase
                                             , text "before:" <+> ppr ty
                                             , text "after:" <+> ppr eTy])
      ; return eTy
      }
  
-- Generates all the f @@ a constraints
genAtAtConstraintsTcM :: Bool -> Type ->  TcM WfElabTypeDetails
genAtAtConstraintsTcM isTyConPhase ty = genAtAtConstraintsExceptTcM isTyConPhase [] [] ty

-- | Elaborate the type with well formed constraints
--   Also collapse the ones that we know are ()'s
elabWithAtAtConstraintsTopTcM :: Bool -> Type -> TcM Type
elabWithAtAtConstraintsTopTcM isTyConPhase ty =
  do { elabd <- genAtAtConstraintsTcM isTyConPhase ty
     ; wfcs <- concatMapM flatten_atat_constraint (newPreds elabd)
     ; traceTc "wf constraints after elab & flattening" (vcat [ppr (newPreds elabd), ppr wfcs])
     ; return $ attachConstraints wfcs (elabTy elabd) }    

-- Generates f @@ a constraints unless tycon passed in appears in LHS
genAtAtConstraintsExceptTcM :: Bool -> [TyCon] -> [Type] -- Things to skip 
                            -> Type ->  TcM WfElabTypeDetails
genAtAtConstraintsExceptTcM isTyConPhase tycons ts ty
  | (TyVarTy _) <- ty = return $ elabDetails ty []
  -- | isLiftedRuntimeRep ty || isLiftedTypeKind ty = return (ty, [])
  -- it  generates (->) @@ a and (a ->) @@ b and recursively generates constraints for a and b
  -- it is a special case of Type constructor
  | (FunTy VisArg v ty1 ty2) <- ty = do -- atc <- (funAt ty1 ty2)
      elabd1 <- (genAtAtConstraintsExceptTcM isTyConPhase tycons ts ty1)
      elabd2 <- (genAtAtConstraintsExceptTcM isTyConPhase tycons ts ty2)
      return $ elabDetails (FunTy VisArg v (elabTy elabd1) (elabTy elabd2))
                           (mergeAtAtConstraints (newPreds elabd1) (newPreds elabd2))

  | (FunTy InvisArg _ constraint ty') <- ty  = do -- we have effectively lost v here becuase i am lazy and we don't care about linear constraints
      elabd_cs <- genAtAtConstraintsExceptTcM False tycons ts constraint
      elabd <- (genAtAtConstraintsExceptTcM isTyConPhase tycons ts ty')
      let rty =  mkInvisFunTysMany (mergeAtAtConstraints (newPreds elabd_cs) [constraint]) (elabTy elabd)
      return $ elabDetails rty (newPreds elabd)

  -- recursively build @@ constraints for type constructor
  | (TyConApp tyc tycargs) <- ty =
      if tyc `hasKey` typeRepTyConKey || isWFMirrorTyCon tyc
      -- this is supposed to save us from sometyperep, typerep nonsense.
        then return $ elabDetails ty []
        else do
        { elabTys_and_atats <- mapM (genAtAtConstraintsExceptTcM isTyConPhase (tyc:tycons) ts) tycargs
        ; let (elab_tys, atc_args) = unzip $ fmap (\d -> (elabTy d, newPreds d)) elabTys_and_atats
        ; if any (== tyc) tycons
          then return $ elabDetails (TyConApp tyc elab_tys) (foldl mergeAtAtConstraints [] atc_args)
          else do { atc_tycon <- tyConGenAtsTcM isTyConPhase tycons ts tyc elab_tys
                  ; return $ elabDetails (TyConApp tyc elab_tys) (foldl mergeAtAtConstraints atc_tycon atc_args)
                  }
        }
  -- for type application we need ty1 @@ ty2 (unless ty2 is * then skip it or ty2 has a constraint kind)
  | (AppTy ty1 ty2) <- ty =
      -- if isHigherKinded ty1 -- Don't break * types apart as we don't have a theory for that yet
      -- then return (ty, [])
      -- else
        if any (ty2 `tcEqType`) (star:ts)
        then do { traceTc "wfelab appty1" (ppr ty1 <+> ppr ty2)
                ; elabd1 <- genAtAtConstraintsExceptTcM isTyConPhase tycons ts ty1
                -- ; elabd2 <- genAtAtConstraintsExceptTcM isTyConPhase tycons ts ty2
                ; return $ elabDetails (AppTy (elabTy elabd1) ty2) (newPreds elabd1)
                }
        else if (head (reverse (snd (tcSplitAppTys (tcTypeKind ty1))))) `tcEqType` constraintKind then
          -- given that we are elaborating over class constraints, we won't want to obtain a c @ x
          --  where c :: k -> *
          do { traceTc "wfelab appty2:" (ppr (tcSplitAppTys (tcTypeKind ty1))
                                         <+> ppr (head (reverse (snd (tcSplitAppTys (tcTypeKind ty1))))))
             ; elabd1 <- genAtAtConstraintsExceptTcM isTyConPhase tycons ts ty1
             ; elabd2 <- genAtAtConstraintsExceptTcM isTyConPhase tycons ts ty2
             ; return $ elabDetails (AppTy (elabTy elabd1) (elabTy elabd2))
                 (mergeAtAtConstraints (newPreds elabd1) (newPreds elabd2))
             }
             else do { traceTc "wfelab appty3:" (ppr (tcSplitAppTys (tcTypeKind ty1))
                                         <+> ppr (head (reverse (snd (tcSplitAppTys (tcTypeKind ty1))))))
                     ; let atc = ty1 `at'at` ty2
                     ; wfc <- flatten_atat_constraint atc -- if it is reducible, reduce it! 
                     ; elabd1 <- genAtAtConstraintsExceptTcM isTyConPhase tycons ts ty1
                     ; elabd2 <- genAtAtConstraintsExceptTcM isTyConPhase tycons ts ty2
                     ; return $ elabDetails (AppTy (elabTy elabd1) (elabTy elabd2))
                       (mergeAtAtConstraints wfc $ mergeAtAtConstraints (newPreds elabd1) (newPreds elabd2))
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
   | (ForAllTy bndr ty1) <- ty = do
      let bvar = binderVar bndr
          bvarTy = mkTyVarTy bvar
          shouldn'tAtAt = isInvisibleArgFlag (binderArgFlag bndr) -- need a more appropriate check here.
      elabd <- if shouldn'tAtAt
               then genAtAtConstraintsExceptTcM isTyConPhase tycons (bvarTy : ts) ty1
               else genAtAtConstraintsExceptTcM isTyConPhase tycons ts ty1
      c_extra <- concatMapM flatten_atat_constraint (newPreds elabd)
      -- let (have'bvar, donthave'bvar) = partition (predHas bvarTy) c_extra
      let r_ty = ForAllTy bndr (attachConstraints c_extra (elabTy elabd))
      -- it is unlikely that we find a type application that is _not_ related to this binder.
      -- May have to change this later
      return $ elabDetails r_ty [] -- genAtAtConstraints ty'

   | CastTy ty1 kco <- ty = do
       elabd <- genAtAtConstraintsExceptTcM isTyConPhase tycons ts ty1
       return $ elabDetails (CastTy (elabTy elabd) kco) (newPreds elabd)
       
   | otherwise = do
      traceTc "wfelab unknown case or nothing to do: " (ppr ty)
      return $ elabDetails ty []

  -- where predHas :: Type -> PredType -> Bool
  --       predHas tv pred = or [eqType tv x | x <- (predTyArgs pred)] -- no me likey


isTyConInternal :: TyCon -> Bool
isTyConInternal tycon =
  tycon `hasKey` tYPETyConKey || tycon `hasKey` runtimeRepTyConKey
  -- || tycon `hasKey` repTyConKey || tycon `hasKey` rep1TyConKey
  -- || tycon `hasKey` typeRepTyConKey
  -- || tycon `hasKey` typeableClassKey
  || tycon `hasKey` eqTyConKey || tycon `hasKey` heqTyConKey
  || tycon `hasKey` someTypeRepTyConKey
  || tycon `hasKey` proxyPrimTyConKey
  || tycon `hasKey` ioTyConKey -- || (tyConName tycon == ioTyConName)
  || tycon `hasKey` listTyConKey
  || tycon `hasKey` maybeTyConKey
  || isBoxedTupleTyCon tycon || isUnboxedTupleTyCon tycon
  || isUnboxedSumTyCon tycon
  || tycon `hasKey` stablePtrPrimTyConKey || tycon `hasKey` stablePtrTyConKey
  || tycon `hasKey` staticPtrTyConKey || (tyConName tycon == staticPtrTyConName)
  || tycon `hasKey` staticPtrInfoTyConKey || (tyConName tycon == staticPtrInfoTyConName)
  || tycon `hasKey` ptrTyConKey || tycon `hasKey` funPtrTyConKey
  || tycon `hasKey` qTyConKey || tyConName tycon == qTyConName
  -- || tycon `hasKey` tExpTyConKey
  || tycon == funTyCon
  || isWfTyCon tycon
  || isWFMirrorTyCon tycon

saneTyConForElab :: TyCon -> Bool
saneTyConForElab tycon =
  not (isUnboxedTupleTyCon tycon
       || isPrimTyCon tycon
       || isPromotedDataCon tycon
       || isFunTyCon tycon
       || isDataFamilyTyCon tycon
       || isClosedTypeFamilyTyCon tycon
      )


-- recursively generates @@ constraints for a type constructor
-- Also rewrite Type family constructors
tyConGenAtsTcM :: Bool
               -> [TyCon]
               -> [Type] --- things to ignore
               -> TyCon
               -> [Type]
               -> TcM ThetaType
tyConGenAtsTcM isTyConPhase eTycons ts tycon args
  | isWFMirrorTyCon tycon -- leave the wftycons untouched
  = do { traceTc "wfelab mirrorTyCon" (ppr tycon); return [] }
  | isTyConInternal tycon || isClassTyCon tycon
  = do { traceTc "wfelab internalTyCon/ClassTyCon" (ppr tycon)
       ; elabds <- mapM (genAtAtConstraintsExceptTcM False (tycon:eTycons) ts) args
       ; let css = fmap newPreds elabds
       ; return $ foldl mergeAtAtConstraints [] css
       }
  | not (saneTyConForElab tycon)
  = if isTyConPhase then return [] -- if we are defining a datatype, we force users to write the constraints
    else do { elabds <- mapM (genAtAtConstraintsExceptTcM False (tycon:eTycons) ts) args
            ; let css = fmap newPreds elabds
            ; return $ foldl mergeAtAtConstraints [] css
            }
  | isTypeSynonymTyCon tycon =
      do { traceTc "wfelab typesyn" (ppr tycon)
    -- if not isTyConPhase
    -- then -- the interaction of typesynonyms and type family is effed up
         ; if (args `lengthAtLeast` (tyConArity tycon))
           then case coreView (mkTyConApp tycon args) of
                  Just ty   -> do { elabd <- genAtAtConstraintsExceptTcM isTyConPhase eTycons ts ty
                                  ; traceTc "tysyn tyConGenAtsTcM: " (ppr (elabTy elabd))
                                  ; concatMapM flatten_atat_constraint $ newPreds elabd }
                  Nothing   -> pprPanic "tysyn tyConGenAts" (ppr tycon)
           else failWithTc (tyConArityErr tycon args)
         }

  | isTyConAssoc tycon && not (isClosedTypeFamilyTyCon tycon) && not (isWFMirrorTyCon tycon)
  = do { traceTc "wfelab isTyConAssoc" (ppr tycon)
       ; let extra_args_tc = drop (tyConArity tycon)
                                  (zip3 args (tyConBinders tycon) (tyConRoles tycon))
             args_tc = take (tyConArity tycon) args
       ; let wftycon = wfMirrorTyCon tycon
       ; traceTc "wfelab lookup2" (ppr wftycon <+> ppr (tyConArity wftycon))
       ; elabds <- mapM (genAtAtConstraintsExceptTcM False (tycon:eTycons) ts) args
       ; let css = fmap newPreds elabds
             wftct = mkTyConApp wftycon args_tc
       ; extra_css <- recGenAts' tycon extra_args_tc args_tc [] []
       ; return $ foldl mergeAtAtConstraints (wftct:extra_css) css
       }
  | isOpenFamilyTyCon tycon
  = do { traceTc "wfelab open fam tycon" (ppr tycon)
       ; elabtys_and_css <- mapM (genAtAtConstraintsExceptTcM isTyConPhase eTycons ts) args
       ; let css = fmap newPreds elabtys_and_css
       ; co_ty_mb <- matchFamTcM tycon args
              
       ; let wftycon = wfMirrorTyCon_maybe tycon
       ; let tfwfcts::ThetaType = maybeToList $ fmap (\t -> mkTyConApp t args) wftycon
       ; traceTc "wfelab open tycon" (vcat [ ppr tycon
                                           , ppr (wfMirrorTyCon_maybe tycon)
                                           , ppr tfwfcts])
       ; case co_ty_mb of
           Nothing -> return $ foldl mergeAtAtConstraints tfwfcts css
           Just r | ty <- reductionReducedType r -> do {
             ; elabd <- genAtAtConstraintsTcM isTyConPhase ty
             ; return $ foldl mergeAtAtConstraints (mergeAtAtConstraints tfwfcts $ newPreds elabd) css
             }
       }

      
  | isTypeFamilyTyCon tycon
    || isDataFamilyTyCon tycon
  = do { traceTc "wfelab datafam/typefam tycon" (ppr tycon)
       ; elabtys_and_css <- mapM (genAtAtConstraintsExceptTcM isTyConPhase eTycons ts) args
       ; let css = fmap newPreds elabtys_and_css
       ; co_ty_mb <- matchFamTcM tycon args
       ; case co_ty_mb of
           Nothing -> return $ foldl mergeAtAtConstraints [] css
           Just r | ty <- reductionReducedType r -> do {
             ; elabd <- genAtAtConstraintsTcM isTyConPhase ty
             ; return $ foldl mergeAtAtConstraints (newPreds elabd) css
             }
       }
  -- How should newtype deriving work, how does coercing constraints work? i think they should be OK... 
  | isNewTyCon tycon, not isTyConPhase =
      do {traceTc "wfelab new tycon" (ppr tycon)
           -- let dc = tyConSingleDataCon tycon
          -- args = fmap scaledThing $ dataConOrigArgTys dc
         ; wfcs <- recGenAtsTcM tycon args ts
         ; elabds <- mapM (genAtAtConstraintsExceptTcM isTyConPhase eTycons ts) args
         ; return $ foldl mergeAtAtConstraints wfcs (fmap newPreds elabds)
         } 
  -- Vanilla type constructor, everything is total
  | otherwise = do { traceTc "wfelab fallthrough:" (ppr tycon)
                   ; recGenAtsTcM tycon args ts}


recGenAtsTcM :: TyCon -> [Type]
             -> [Type] -- things to ignore
             -> TcM ThetaType
recGenAtsTcM tc args ts = do wfcs <- recGenAts tc args ts
                             concatMapM flatten_atat_constraint wfcs

recGenAts :: Monad m => TyCon -> [Type]
          -> [Type] -- things to ignore
          -> m ThetaType
recGenAts tc args ts = recGenAts' tc arg_binder_role [] [] ts
  where
    binders = tyConBinders tc
    roles = tyConRoles tc
    arg_binder_role = zip3 args binders roles

recGenAts' :: Monad m => TyCon
                      -> [(Type, TyConBinder, Role)] -- remaning 
                      -> [Type]                -- done
                      -> ThetaType             -- accumuator
                      -> [Type]                -- things to ignore
                      -> m ThetaType
recGenAts' _ [] _ acc _ = return acc
recGenAts' tyc ((hd, bndr, r) : tl) tycargs' acc ts
  = do { let atc = if (isNamedTyConBinder bndr) -- TODO: I think there is a cannonical way to do this check.
                      || isInvisibleArgFlag (tyConBinderArgFlag bndr)
                      || any (eqType hd) (star:ts) -- we don't want f @@ * creaping in
                      -- || r == Phantom
                   then []
                   else [(mkTyConApp tyc (tycargs')) `at'at` hd]
       ; recGenAts' tyc tl (tycargs' ++ [hd]) (mergeAtAtConstraints acc atc) ts
       }


-- takes in type arguments ty1 ty2 and returns the constraint ty1 @@ ty2
-- we do have to compute the kinds for ty1 and ty2 so
-- that the correctly kinded type is instantiated in @@ class
-- @@ {k'} {k} (f :: k' -> k) (arg:: k')
at'at :: Type -> Type -> PredType
at'at f arg = mkTyConApp wfTyCon [argk, resk, f, arg]
  where argk = tcTypeKind arg
        fk   = tcTypeKind f
        resk = piResultTy fk argk

-- sequenceAtAts :: Monad m
--               => TyCon -- base tycon
--               -> [Type] -- done
--               -> [Type] -- to process
--               -> m ThetaType
-- sequenceAtAts btc args extra_args = sequenceAtAts_aux [] btc args extra_args
--   where
--     sequenceAtAts_aux :: Monad m => ThetaType -> TyCon -> [Type] -> [Type] -> m ThetaType
--     sequenceAtAts_aux acc _ _ [] = return acc
--     sequenceAtAts_aux acc btc args (x:xs) = do
--       let new_at = (mkTyConApp btc args) `at'at` x
--       sequenceAtAts_aux (new_at:acc) btc (args ++ [x]) xs
  


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
  | (FunTy _ _ argTy resTy) <- ty
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
  | (FunTy _ _ argTy resTy) <- ty
  = predTyVars argTy ++ predTyVars resTy
  | otherwise = []

-- | eagerly beat the type to its normal form then if its an empty dictonary return [] else return the actual dictonary
--   the caller of the function doesn't expect any redundant @@'s to be left beind, unless it is a higer order
--   data type. eg. data T f b = MkT f b (f b)
--   MkT :: f @@ b => f -> b -> T f b
--   wf-ctxt (MkT) = f @@ b and we cannot do anything with it unless f is instantiated
flatten_atat_constraint :: Type -> TcM [Type]
flatten_atat_constraint ty@(TyConApp tc _)
  | -- tc == wfTyCon ||
    isTypeFamilyTyCon tc
  = do fam_envs <- GHC.Tc.Instance.Family.tcGetFamInstEnvs
       let ty' = topNormaliseType fam_envs ty
       tuplesToList ty'
flatten_atat_constraint ty = return [ty]

tuplesToList :: Type -> TcM [Type]
tuplesToList ty
  | (TyConApp tc tork_args) <- ty, isCTupleTyConName $ tyConName tc =
      do return tork_args
  | otherwise = return [ty]

-- This is never called in Tycon Defining phase. so look break all barriers
-- Generates all the f @@ a constraints
genWfConstraints :: (
#if MIN_VERSION_base(4,16,0)
                     Total m,
#endif
                 Monad m) => Type -> [Type] ->  m ThetaType
genWfConstraints ty skiptys = do d <- genAtAtConstraintsExcept [] skiptys ty
                                 return $ newPreds d

-- This better not be used with a foralled type. It may break things or may not elaborate at all.
elabAtAtConstraints :: (
#if MIN_VERSION_base(4,16,0)
    Total m,
#endif
  Monad m, MonadIO m) => Type ->  m Type
elabAtAtConstraints ty = do elabd <- genAtAtConstraintsExcept [] [] ty
                            return $ attachConstraints (newPreds elabd) (elabTy elabd)
                            
-- Generates f @@ a constraints unless tycon passed in appears in LHS
genAtAtConstraintsExcept :: (
#if MIN_VERSION_base(4,16,0)
    Total m,
#endif
  Monad m) => [TyCon] -> [Type] -> Type ->  m WfElabTypeDetails
genAtAtConstraintsExcept tycons ts ty
  -- | isLiftedRuntimeRep ty || isLiftedTypeKind ty = return (ty, []) -- i don't think we need this now.
  -- it  generates (->) @@ a and (a ->) @@ b and recursively generates constraints for a and b
  -- it is a special case of Type constructor
  | (FunTy VisArg v ty1 ty2) <- ty = do -- atc <- (funAt ty1 ty2)
      elabd1 <- genAtAtConstraintsExcept tycons ts ty1
      elabd2 <- genAtAtConstraintsExcept tycons ts ty2
      return $ elabDetails (FunTy VisArg v (elabTy elabd1) (elabTy elabd2)) (mergeAtAtConstraints (newPreds elabd1) (newPreds elabd2))

    -- for (=>) types ignore the 1st arg as it is a constraint
  | (FunTy InvisArg v constraint ty') <- ty  = do
      elabd <- genAtAtConstraintsExcept tycons ts ty'
      return$ elabDetails (FunTy InvisArg v constraint (elabTy elabd)) (newPreds elabd)

  -- recursively build @@ constraints for type constructor
  | (TyConApp tyc tycargs) <- ty =
      if tyc `hasKey` typeRepTyConKey -- this is supposed to save us from sometyperep, typerep nonsense.
        then return $ elabDetails ty []
        else do
        { elabTys_and_atats <- mapM (genAtAtConstraintsExcept (tyc:tycons) ts) tycargs
        ; let (elab_tys, atc_args) = unzip $ fmap (\d -> (elabTy d, newPreds d)) elabTys_and_atats
        ; if any (== tyc) tycons
          then return $ elabDetails (TyConApp tyc elab_tys) (foldl mergeAtAtConstraints [] atc_args)
          else do { atc_tycon <- tyConGenAts tycons ts tyc elab_tys
                  ; return $ elabDetails (TyConApp tyc elab_tys) (foldl mergeAtAtConstraints atc_tycon atc_args)
                  }
        }

  -- for type application we need ty1 @@ ty2
  -- for type application we need ty1 @@ ty2 (unless ty2 is * then skip it or ty2 has a constraint kind)
  | (AppTy ty1 ty2) <- ty =
      -- if isHigherKinded ty1 -- Don't break * types apart as we don't have a theory for that yet
      -- then return (ty, [])
      -- else
        if (any (eqType ty2) (star:ts))
        then do { elabd <- genAtAtConstraintsExcept tycons ts ty1
                ; return $ elabDetails (AppTy (elabTy elabd) ty2) (newPreds elabd)
                }
        else if (fst $ tcSplitAppTys (tcTypeKind ty1)) `tcEqType` constraintKind then
          -- this function is going to make our implimentation really, really slow.
          -- given that we split the TyApps into a list we should just foldr over the args to get the constraints..
          -- given that we are elaborating over class constraints, we won't want to obtain a c @ x
          --  where c :: k -> *
               do { -- traceTc "wfelab appty:" (ppr $ tcSplitAppTys ty1)
                    elabd1 <- genAtAtConstraintsExcept tycons ts ty1
                  ; elabd2 <- genAtAtConstraintsExcept tycons ts ty2
                  ; return $ elabDetails (AppTy (elabTy elabd1) (elabTy elabd2))
                    (mergeAtAtConstraints (newPreds elabd1) (newPreds elabd2))
                  }
        else do { let atc = [ty1 `at'at` ty2]
                ; elabd1 <- genAtAtConstraintsExcept tycons ts ty1
                ; elabd2 <- genAtAtConstraintsExcept tycons ts ty2
                ; return $ elabDetails (AppTy (elabTy elabd1) (elabTy elabd2))
                       (mergeAtAtConstraints atc $
                         mergeAtAtConstraints (newPreds elabd1) (newPreds elabd2))
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
      elabd <- if shouldn'tAtAt
               then genAtAtConstraintsExcept tycons (bvarTy : ts) ty1
               else genAtAtConstraintsExcept tycons ts ty1
      -- let (have'bvar, donthave'bvar) = partition (predHas bvarTy) (newPreds elabd) 
      let r_ty = ForAllTy bndr (attachConstraints (newPreds elabd) (elabTy elabd))
      return $ elabDetails r_ty []

  | otherwise =
      -- do traceTc "wfelab unknown case or nothing to do: " (ppr ty)
      return $ elabDetails ty []

  -- where predHas :: Type -> PredType -> Bool
  --       predHas tv pred = or [eqType tv x | x <- (predTyArgs pred)] -- no me likey

-- recursively generates @@ constraints for a type constructor
-- Doesn't rewrite type family constructors
tyConGenAts :: (
#if MIN_VERSION_base(4,16,0)
                Total m,
#endif
               Monad m) -- TODO: This is under constrained, we need to add more context to this monad.
            => [TyCon]
            -> [Type] -- things to ignore
            -> TyCon -> [Type] -> m ThetaType
tyConGenAts eTycons ts tycon args
  | isTyConInternal tycon || isGadtSyntaxTyCon tycon 
  = concatMapM (\ x -> genWfConstraints x ts) args
  | isTyConAssoc tycon && not (isNewTyCon tycon)
  = do { let (args', extra_args) = splitAt (length (tyConVisibleTyVars tycon))
                                   (zip3 args (tyConBinders tycon) (tyConRoles tycon))
       ; recGenAts' tycon extra_args (map (\(e, _, _) -> e) args') [] ts
       }
  | isTypeSynonymTyCon tycon =
      if (args `lengthAtLeast` (tyConArity tycon))
      then case coreView (TyConApp tycon args) of
             Just ty   -> do { elabd <- genAtAtConstraintsExcept eTycons ts ty
                             -- ; traceTc "tysyn tyConGenAts: " (ppr ty)
                             ; return $ newPreds elabd }
             Nothing   -> pprPanic "tysyn tyConGenAts" (ppr tycon)
      else pprPanic "tysyn tyConGenAts" (ppr tycon)
  | isNewTyCon tycon =  -- Look through newtypes if its not a definition phase
      do { wfcs <- recGenAts tycon args ts  
         ; elabds <- mapM (genAtAtConstraintsExcept eTycons ts) args
         ; return $ foldl mergeAtAtConstraints wfcs (fmap newPreds elabds)
         }
  | not (saneTyConForElab tycon)
  = do { elabtys_and_css <- mapM (genAtAtConstraintsExcept (tycon:eTycons) ts) args
       ; let css = fmap newPreds elabtys_and_css
       ; return $ foldl mergeAtAtConstraints [] css
       }
  | isFamilyTyCon tycon -- Ideally look up the TyFam mirror constraints @Alex
  = do { elabtys_and_css <- mapM (genAtAtConstraintsExcept (tycon:eTycons) ts) args
       ; let css = fmap newPreds elabtys_and_css
       ; return $ foldl mergeAtAtConstraints [] css
       }
  | otherwise = recGenAts tycon args ts
