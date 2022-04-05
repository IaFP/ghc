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
  , genWfConstraints -- lifted version of genWfConstraintsTcM needed for DerivM 
  , predTyArgs, predTyVars
  , attachConstraints, mergeAtAtConstraints
  , saneTyConForElab

  -- ** TcM functions
  , elabWfTypeTcM -- main work horse
  , simplifyWfTypeTcM 
  , genWfConstraintsTcM, genAtAtConstraintsTcM
  , genAtAtConstraintsExceptTcM
  , elabWithAtAtConstraintsTopTcM
  , flatten_atat_constraint, at'at
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
import GHC.Builtin.Types (isCTupleTyConName, wfTyCon)
import GHC.Tc.Utils.Monad
import GHC.Utils.Panic (pprPanic)
import GHC.Utils.Outputable
import GHC.Utils.Misc(lengthAtLeast)

import Control.Monad.Trans.Class

-------------------------------------------------------------------------
{-
%************************************************************************
%*                                                                      *
             Generating Wellformedness (@) Constraints
*                                                                      *
************************************************************************

-}
-- Note [isTyConPhase]
-- This is to mark that we are elaborating the type constructors.
-- setting isTyConPhase to false means that we are elaborating a type signature
-- Not really. False = we are doing to egarly reduce the @ constraints
--             True = we are going to let the at constraint be as is

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

-- | alias for the * that is used at type level
star :: Type
star = liftedTypeKind

-- | Elaborate the type with well formed constraints
-- ANI TODO instead of a Bool take in a UserTypeContext.
-- ANI TODO: This should also be called elabWfType as it elaborates a type.
-- That will help decided whether we should be elaborating/eagerly reducing @ constraints etc.
elabWfTypeTcM :: Bool -> Type -> TcM Type
elabWfTypeTcM isTyConPhase ty
  | isForAllTy ty = do
      { let (covarbndrs, ty') = splitForAllTyCoVarBinders ty
      ; elabd <- genAtAtConstraintsTcM isTyConPhase ty'
      ; c_extra' <- if isTyConPhase
                    then return (newPreds elabd)
                    else do css <- mapM flatten_atat_constraint (newPreds elabd)
                            return $ foldl mergeAtAtConstraints [] css
      ; let eTy = mkForAllTys covarbndrs $ attachConstraints c_extra' (elabTy elabd)
      ; traceTc "wfelabtype(foralled)=" (vcat [ text "tyconphase" <+> ppr isTyConPhase
                                              , text "before:" <+> ppr ty
                                              , text "after:" <+> ppr eTy])
      ; return eTy
      }
  | otherwise = do
      { eTy <- elabWithAtAtConstraintsTopTcM isTyConPhase ty
      ; traceTc "wfelabtype(vanilla)=" (vcat [ text "tyconphase" <+> ppr isTyConPhase
                                             , text "before:" <+> ppr ty
                                             , text "after:" <+> ppr eTy])
      ; return eTy
      }
  
-- Generates all the f @ a constraints
genAtAtConstraintsTcM :: Bool -> Type -> TcM WfElabTypeDetails
genAtAtConstraintsTcM isTyConPhase ty = genAtAtConstraintsExceptTcM isTyConPhase [] [] ty

genWfConstraintsTcM :: Bool -> Type -> [Type] -> TcM ThetaType
genWfConstraintsTcM isTyConPhase ty stys = do elabd <- genAtAtConstraintsExceptTcM isTyConPhase [] stys ty
                                              return $ newPreds elabd

-- | Elaborate the type with well formed constraints
--   Also collapse the ones that we know are ()'s
elabWithAtAtConstraintsTopTcM :: Bool -> Type -> TcM Type
elabWithAtAtConstraintsTopTcM isTyConPhase ty =
  do { elabd <- genAtAtConstraintsTcM isTyConPhase ty
     ; wfcs <- if isTyConPhase then return (newPreds elabd)
                               else do css <- mapM flatten_atat_constraint (newPreds elabd)
                                       return $ foldl mergeAtAtConstraints [] css 
     ; return $ attachConstraints wfcs (elabTy elabd) }    

-- Generates f @@ a constraints unless tycon passed in appears in LHS
genAtAtConstraintsExceptTcM :: Bool -> [TyCon] -> [Type] -- Things to skip 
                            -> Type ->  TcM WfElabTypeDetails
genAtAtConstraintsExceptTcM isTyConPhase tycons ts ty
  -- Do nothing for type variables
  | (TyVarTy _) <- ty = return $ elabDetails ty []

  | (FunTy VisArg v ty1 ty2) <- ty = do -- atc <- (funAt ty1 ty2)
      elabd1 <- (genAtAtConstraintsExceptTcM isTyConPhase tycons ts ty1)
      elabd2 <- (genAtAtConstraintsExceptTcM isTyConPhase tycons ts ty2)
      return $ elabDetails (FunTy VisArg v (elabTy elabd1) (elabTy elabd2))
                           (mergeAtAtConstraints (newPreds elabd1) (newPreds elabd2))

  | (FunTy InvisArg _ constraint ty') <- ty  = do
      -- we have effectively lost v here becuase i am lazy and we don't care about linear constraints
      -- Actually even those guys don't care about linear constraints hehehe.
      -- They are always considered to have a Many multiplicity
      elabd_cs <- genAtAtConstraintsExceptTcM True tycons ts constraint
      elabd <- genAtAtConstraintsExceptTcM isTyConPhase tycons ts ty'
      let cs' = mergeAtAtConstraints (newPreds elabd_cs) [constraint]
      let rty =  mkInvisFunTysMany cs' (elabTy elabd)
      return $ elabDetails rty (newPreds elabd)

  -- recursively build @ constraints for type constructor
  | (TyConApp tyc tycargs) <- ty =
      if tyc `hasKey` typeRepTyConKey  -- this is supposed to save us from sometyperep, typerep nonsense.
      || isWFMirrorTyCon tyc
      || any (== tyc) tycons
        then return $ elabDetails ty []
        else if tyConResKind tyc `tcEqType` constraintKind
          -- this is a class tcTyCon or a constraint kind tycon. we don't want to generate tyc @ arg for such tyc
             then do { atc_tycon <- tyConGenAtsTcM isTyConPhase (tyc:tycons) ts tyc tycargs
                     ; return $ elabDetails (TyConApp tyc tycargs) atc_tycon
                     }
             else do { atc_tycon <- tyConGenAtsTcM isTyConPhase tycons ts tyc tycargs
                     ; return $ elabDetails (TyConApp tyc tycargs) atc_tycon
                     }
        
  -- for type application we need ty1 @ ty2 (unless ty2 is * then skip it, or ty2 has a constraint kind)
  | (AppTy ty1 ty2) <- ty = -- ANI TODO: simplify this nonsense. 
        if ty2 `tcEqType` star
        then do { traceTc "wfelab appty1" (ppr ty1 <+> ppr ty2)
                ; elabd1 <- genAtAtConstraintsExceptTcM isTyConPhase tycons ts ty1
                ; return $ elabDetails (AppTy (elabTy elabd1) ty2) (newPreds elabd1)
                }
        else if (head (reverse (snd (tcSplitAppTys (tcTypeKind ty1))))) `tcEqType` constraintKind then
          -- given that we are elaborating over class constraints, we won't want to obtain a c @ x
          --  where c :: k -> Constraint
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
                     ; wfc <- if isTyConPhase
                              then return [atc]
                              else flatten_atat_constraint atc -- if it is reducible, reduce it! 
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
      c_extra <- if isTyConPhase
                 then return (newPreds elabd)
                 else do css <- mapM flatten_atat_constraint $ newPreds elabd
                         return $ foldl mergeAtAtConstraints [] css
                             
      -- let (have'bvar, donthave'bvar) = partition (predHas bvarTy) c_extra
      let r_ty = ForAllTy bndr (attachConstraints c_extra (elabTy elabd))
      -- it is unlikely that we find a type application that is _not_ related to this binder.
      -- May have to change this later
      return $ elabDetails r_ty [] -- genAtAtConstraints ty'

   | CastTy ty1 kco <- ty = do
       elabd <- genAtAtConstraintsExceptTcM isTyConPhase tycons ts ty1
       return $ elabDetails (CastTy (elabTy elabd) kco) (newPreds elabd)
       
   | otherwise = do -- Type Lits CoersionTy
      traceTc "wfelab unknown case or nothing to do: " (ppr ty)
      return $ elabDetails ty []

  -- where predHas :: Type -> PredType -> Bool
  --       predHas tv pred = or [eqType tv x | x <- (predTyArgs pred)] -- no me likey


isTyConInternal :: TyCon -> Bool
isTyConInternal tycon =
  tycon `hasKey` tYPETyConKey || tycon `hasKey` runtimeRepTyConKey
  || tycon `hasKey` someTypeRepTyConKey
  -- || tycon `hasKey` repTyConKey || tycon `hasKey` rep1TyConKey
  -- || tycon `hasKey` typeRepTyConKey
  -- || tycon `hasKey` typeableClassKey
  || tycon `hasKey` eqTyConKey || tycon `hasKey` heqTyConKey
  || tycon `hasKey` proxyPrimTyConKey
  -- || tycon `hasKey` ioTyConKey -- || (tyConName tycon == ioTyConName)
  || tycon `hasKey` listTyConKey
  || tycon `hasKey` maybeTyConKey
  || isBoxedTupleTyCon tycon || isUnboxedTupleTyCon tycon
  || isUnboxedSumTyCon tycon
  || tycon `hasKey` stablePtrPrimTyConKey || tycon `hasKey` stablePtrTyConKey
  -- || tycon `hasKey` staticPtrTyConKey || (tyConName tycon == staticPtrTyConName)
  || tycon `hasKey` staticPtrInfoTyConKey || (tyConName tycon == staticPtrInfoTyConName)
  || tycon `hasKey` ptrTyConKey || tycon `hasKey` funPtrTyConKey
  || tycon `hasKey` qTyConKey || tyConName tycon == qTyConName
  || tycon == funTyCon
  || isWFMirrorTyCon tycon -- @ is also a mirror

-- ANI ToDo, this function is no longer meant to do what it's supposed to do. Need to rethink this one.
saneTyConForElab :: TyCon -> Bool
saneTyConForElab tycon =
  not (isUnboxedTupleTyCon tycon
       || isPrimTyCon tycon
       || isPromotedDataCon tycon
       || isFunTyCon tycon
      )


-- recursively generates @ constraints for a type constructor
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
  | isTyConInternal tycon || isClassTyCon tycon || tyConResKind tycon `tcEqType` constraintKind
  = do { traceTc "wfelab internalTyCon/ClassTyCon/ConstraintKind tycon" (ppr tycon)
       ; elabds <- mapM (genAtAtConstraintsExceptTcM False eTycons ts) args
       ; let css = fmap newPreds elabds
       ; return $ foldl mergeAtAtConstraints [] css
       }
  | not (saneTyConForElab tycon)
  = if isTyConPhase then return [] -- if we are defining a datatype, we force users to write the constraints
    else do { elabds <- mapM (genAtAtConstraintsExceptTcM False (tycon:eTycons) ts) args
            ; return $ foldl mergeAtAtConstraints [] $ fmap newPreds elabds
            }
  | isTypeSynonymTyCon tycon =
      do { traceTc "wfelab typesyn" (ppr tycon)
         ; if (args `lengthAtLeast` (tyConArity tycon))
           then case coreView (mkTyConApp tycon args) of
                  Just ty   -> do { elabd <- genAtAtConstraintsExceptTcM isTyConPhase eTycons ts ty
                                  ; if isTyConPhase -- this check is probably not needed as we can never have a typ synonym cycle..
                                    then return (newPreds elabd)
                                    else do css <- mapM flatten_atat_constraint $ newPreds elabd
                                            return $ foldl mergeAtAtConstraints [] css 
                                  }
                  
                  Nothing   -> pprPanic "tysyn tyConGenAts" (ppr tycon)
           else failWithTc (tyConArityErr tycon args)
         }

  | isTyConAssoc tycon
  = do { traceTc "wfelab isTyConAssoc" (ppr tycon <+> ppr args)
       ; let (args_tc, extra_args_tc) = splitAt (tyConArity tycon) args
       ; let wftycon = wfMirrorTyCon tycon -- this better exist
       ; traceTc "wfelab lookup2" (ppr wftycon)
         -- This tycon may be oversaturated so we break down the args into 2 parts:
         -- 1. args_tc which is of length tycon arity
         -- 2. extra_args_tc which is the rest of the args
         -- we would use the wf mirror to generate wf_tc args_tc constraint
         -- the rest will be given to generate wf ((tycon args) extra_args_t)
         -- For example: Rep a :: * -> *
         -- wf (Rep a x) = [$wf'Rep a, Rep a @ x]
         
       ; let wftct = mkTyConApp wftycon args_tc
       ; extra_css <- sequenceAts tycon args_tc extra_args_tc [] []
       ; args_wfts <- mapM (genAtAtConstraintsExceptTcM isTyConPhase eTycons ts) args
       ; extra_css' <- if isTyConPhase then return [wftct:extra_css] else mapM flatten_atat_constraint (wftct:extra_css)
       ; return $ foldl mergeAtAtConstraints [] ((fmap newPreds args_wfts) ++ extra_css')
       }
  | isOpenTypeFamilyTyCon tycon
  = do { traceTc "wfelab open typefam" (ppr tycon <+> ppr args)
       ; let (args_tc, extra_args_tc) = splitAt (tyConArity tycon) args
       ; let wftycon = wfMirrorTyCon tycon -- this better exist
       ; traceTc "wfelab lookup2" (ppr wftycon)
         -- This tycon may be oversaturated so we break down the args into 2 parts:
         -- 1. args_tc which is of length tycon arity
         -- 2. extra_args_tc which is the rest of the args
         -- we would use the wf mirror to generate wf_tc args_tc constraint
         -- the rest will be given to generate wf ((tycon args) extra_args_t)
         -- For example: Rep a :: * -> *
         -- wf (Rep a x) = [$wf'Rep a, Rep a @ x]
         
       ; let wftct = mkTyConApp wftycon args_tc
       ; extra_css <- sequenceAts tycon args_tc extra_args_tc [] []
       ; args_wfts <- mapM (genAtAtConstraintsExceptTcM isTyConPhase eTycons ts) args       
       ; extra_css' <- if isTyConPhase then return [wftct:extra_css] else mapM flatten_atat_constraint (wftct:extra_css)
       ; return $ foldl mergeAtAtConstraints [] ((fmap newPreds args_wfts) ++ extra_css')
       }
  | isTypeFamilyTyCon tycon
    || isDataFamilyTyCon tycon
  = do { traceTc "wfelab datafam/typefam tycon" (ppr tycon)
       ; co_ty_mb <- matchFamTcM tycon args
       ; args_wfts <- mapM (genAtAtConstraintsExceptTcM isTyConPhase eTycons ts) args       
       ; case co_ty_mb of
           Nothing -> return $ foldl mergeAtAtConstraints [] (fmap newPreds args_wfts)
           Just r | ty <- reductionReducedType r -> do {
             ; elabd <- genAtAtConstraintsTcM isTyConPhase ty
             ; return $ foldl mergeAtAtConstraints [] ((fmap newPreds args_wfts) ++ [newPreds elabd])
             }
       }
  -- How should newtype deriving work, how does coercing constraints work? i think they should be OK... 
  | isNewTyCon tycon
  = do { traceTc "wfelab new tycon" (ppr tycon)
       ; wfcs <- recGenAtsTcM tycon args ts
       ; elabds <- mapM (genAtAtConstraintsExceptTcM isTyConPhase eTycons ts) args
       ; return $ foldl mergeAtAtConstraints wfcs (fmap newPreds elabds)
       } 
  -- Vanilla type constructor, everything is total
  | otherwise
  = do { traceTc "wfelab fallthrough:" (ppr tycon)
       ; elabds <- mapM (genAtAtConstraintsExceptTcM isTyConPhase eTycons ts) args
       ; ats <- recGenAtsTcM tycon args ts {-etycons-} 
       ; return $ foldl mergeAtAtConstraints ats $ fmap newPreds elabds }


recGenAtsTcM :: TyCon -> [Type]
             -> [Type] -- things to ignore
             -> TcM ThetaType
recGenAtsTcM tc args ts {-etycons-} = recGenAts tc args ts {-etycons-} 


recGenAts :: Monad m => TyCon -> [Type]
          -> [Type] -- things to ignore
          -> m ThetaType
recGenAts tc args ts {-etycons-} = recGenAts' tc arg_binders [] [] ts
  where
    binders = tyConBinders tc
    arg_binders = zip args binders


-- given TyCon T and type arguments say [a, b, c]
-- we generate [T @ a, T a @ b, T a b @ c]
-- only if it were that simple. Sometimes
-- we need to worry about the invisible type applications
-- especially for GADTs that may be kind polymorphic.
-- Hence we zip the actual arguments with
-- the formal arguments and then ignore the ones that are inferred
-- i.e. automatically put in by the compiler.
-- Fore example T (a::k) b c is actually T k a b c where k is inferred
-- hence we would only generate [T k @ a, T k a @ b, T k a b @ c]. T @ k is skipped
-- ref. see https://github.com/IaFP/PartialityInPractice/issues/2
recGenAts' :: Monad m => TyCon
                      -> [(Type, TyConBinder)] -- remaning 
                      -> [Type]                -- done
                      -> ThetaType             -- accumuator
                      -> [Type]                -- things to ignore
                      -> m ThetaType
recGenAts' _ [] _ acc _ = return acc
recGenAts' tyc ((hd, bndr) : tl) tycargs' acc ts
  | isNamedTyConBinder bndr -- TODO: I think there is a cannonical way to do this check.
    || isInvisibleArgFlag (tyConBinderArgFlag bndr)
    || hd `tcEqType` star  -- we don't want f @@ * creaping in
  = recGenAts' tyc tl (tycargs' ++ [hd]) acc ts
  | otherwise
  = do { let atc = [mkTyConApp tyc tycargs' `at'at` hd]
       ; recGenAts' tyc tl (tycargs' ++ [hd]) (mergeAtAtConstraints acc atc) ts
       -- ANI TODO I don't think we really need to merge here. Just a concat is sufficient.
       }

-- simpler version of recGenAts'
-- We don't have to care about named tycon binders or invisible arguments
-- so simply generate at constraints
sequenceAts :: TyCon -> [Type] -> [Type] -> [Type] -> ThetaType -> TcM ThetaType
sequenceAts _ _ [] _ acc  = return acc
sequenceAts tycon args_tc (ty:extra_args) ts acc
  | any (ty `tcEqType`) (star:ts)
  = sequenceAts tycon (args_tc++[ty]) extra_args ts acc
  | otherwise
  = do let atc = (mkTyConApp tycon args_tc) `at'at` ty
       sequenceAts tycon (args_tc++[ty]) extra_args ts (mergeAtAtConstraints [atc] acc)

-- takes in type arguments ty1 ty2 and returns the constraint ty1 @ ty2
-- we do have to compute the kinds for ty1 and ty2 so
-- that the correctly kinded type is instantiated in @ class
-- @ {k'} {k} (f :: k' -> k) (arg:: k')
at'at :: Type -> Type -> PredType
at'at f arg = mkTyConApp wfTyCon [argk, resk, f, arg]
  where argk = tcTypeKind arg
        fk   = tcTypeKind f
        resk = piResultTy fk argk

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
flatten_atat_constraint :: PredType -> TcM ThetaType
flatten_atat_constraint ty@(TyConApp tc _)
  | -- tc == wfTyCon ||
    isTypeFamilyTyCon tc
  = do fam_envs <- GHC.Tc.Instance.Family.tcGetFamInstEnvs
       let ty' = topNormaliseType fam_envs ty
       tuplesToList ty'
flatten_atat_constraint ty = return [ty]


-- Given a function say forall tvs. wft(T a) => tau
-- we reduce this to forall tvs. wft'(T a) => tau
-- where wft' (T a) = flatten_atat_constraints wft (T a)
simplifyWfTypeTcM  :: Type -> TcM Type
simplifyWfTypeTcM ty = do
  theta' <- foldl mergeAtAtConstraints [] <$> mapM flatten_atat_constraint theta
  traceTc "wfelabtype simplify" (vcat [ text "before:" <+> ppr ty
                                      , text "after:" <+> ppr (mkSpecSigmaTy tvs theta' tau)
                                      ])
  return $ mkSpecSigmaTy tvs theta' tau
    where
      (tvs, theta, tau) = tcSplitSigmaTy ty
  

tuplesToList :: Type -> TcM [Type]
tuplesToList ty
  | (TyConApp tc tork_args) <- ty
  , isCTupleTyConName $ tyConName tc
  = do return tork_args
  | otherwise
  = return [ty]

-- Lifted version of genAtAtConstraintsExceptTcM.
-- Generates all the f @ a constraints in a DeriveM 
genWfConstraints :: MonadTrans t => Bool -> Type -> [Type] ->  t TcM ThetaType
genWfConstraints isTyConPhase ty stys = lift $ genWfConstraintsTcM isTyConPhase ty stys
