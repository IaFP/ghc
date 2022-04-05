{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 903
{-# LANGUAGE QuantifiedConstraints, ExplicitNamespaces, TypeOperators, TypeFamilies #-}
#endif
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}

module GHC.Tc.Gen.Bind
   ( tcLocalBinds
   , tcTopBinds
   , tcValBinds
   , tcHsBootSigs
   , tcPolyCheck
   , chooseInferredQuantifiers
   )
where

import GHC.Prelude

import {-# SOURCE #-} GHC.Tc.Gen.Match ( tcGRHSsPat, tcMatchesFun )
import {-# SOURCE #-} GHC.Tc.Gen.Expr  ( tcCheckMonoExpr )
import {-# SOURCE #-} GHC.Tc.TyCl.PatSyn ( tcPatSynDecl, tcPatSynBuilderBind )

import GHC.Types.Tickish (CoreTickish, GenTickish (..))
import GHC.Types.CostCentre (mkUserCC, CCFlavour(DeclCC))
import GHC.Driver.Session
import GHC.Data.FastString
import GHC.Hs
import GHC.Tc.Errors.Types
import GHC.Tc.Gen.Sig
import GHC.Tc.Utils.Concrete ( hasFixedRuntimeRep )
import GHC.Tc.Utils.Monad
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.Env
import GHC.Tc.Utils.Unify
import GHC.Tc.Solver
import GHC.Tc.Types.Evidence
import GHC.Tc.Gen.HsType
import GHC.Tc.Gen.Pat
import GHC.Tc.Utils.TcMType
import GHC.Core.Reduction ( Reduction(..) )
import GHC.Core.Multiplicity
import GHC.Core.FamInstEnv( normaliseType )
import GHC.Tc.Instance.Family( tcGetFamInstEnvs )
import GHC.Tc.Utils.TcType
import GHC.Core.Type (mkStrLitTy, tidyOpenType, mkCastTy)
import GHC.Builtin.Types ( mkBoxedTupleTy )
import GHC.Builtin.Types.Prim
import GHC.Types.SourceText
import GHC.Types.Id
import GHC.Types.Var as Var
import GHC.Types.Var.Set
import GHC.Types.Var.Env( TidyEnv )
import GHC.Unit.Module
import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Types.Name.Env
import GHC.Types.SrcLoc
import GHC.Data.Bag
import GHC.Utils.Error
import GHC.Data.Graph.Directed
import GHC.Data.Maybe
import GHC.Utils.Misc
import GHC.Types.Basic
import GHC.Types.CompleteMatch
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic
import GHC.Builtin.Names( ipClassName )
import GHC.Tc.Validity (checkValidType)
import GHC.Types.Unique.FM
import GHC.Types.Unique.DSet
import GHC.Types.Unique.Set
import qualified GHC.LanguageExtensions as LangExt
#if MIN_VERSION_base(4,16,0)
import GHC.Types (WFT)
#endif

import Control.Monad
import Data.Foldable (find)

{-
************************************************************************
*                                                                      *
\subsection{Type-checking bindings}
*                                                                      *
************************************************************************

@tcBindsAndThen@ typechecks a @HsBinds@.  The "and then" part is because
it needs to know something about the {\em usage} of the things bound,
so that it can create specialisations of them.  So @tcBindsAndThen@
takes a function which, given an extended environment, E, typechecks
the scope of the bindings returning a typechecked thing and (most
important) an LIE.  It is this LIE which is then used as the basis for
specialising the things bound.

@tcBindsAndThen@ also takes a "combiner" which glues together the
bindings and the "thing" to make a new "thing".

The real work is done by @tcBindWithSigsAndThen@.

Recursive and non-recursive binds are handled in essentially the same
way: because of uniques there are no scoping issues left.  The only
difference is that non-recursive bindings can bind primitive values.

Even for non-recursive binding groups we add typings for each binder
to the LVE for the following reason.  When each individual binding is
checked the type of its LHS is unified with that of its RHS; and
type-checking the LHS of course requires that the binder is in scope.

At the top-level the LIE is sure to contain nothing but constant
dictionaries, which we resolve at the module level.

Note [Polymorphic recursion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The game plan for polymorphic recursion in the code above is

        * Bind any variable for which we have a type signature
          to an Id with a polymorphic type.  Then when type-checking
          the RHSs we'll make a full polymorphic call.

This fine, but if you aren't a bit careful you end up with a horrendous
amount of partial application and (worse) a huge space leak. For example:

        f :: Eq a => [a] -> [a]
        f xs = ...f...

If we don't take care, after typechecking we get

        f = /\a -> \d::Eq a -> let f' = f a d
                               in
                               \ys:[a] -> ...f'...

Notice the stupid construction of (f a d), which is of course
identical to the function we're executing.  In this case, the
polymorphic recursion isn't being used (but that's a very common case).
This can lead to a massive space leak, from the following top-level defn
(post-typechecking)

        ff :: [Int] -> [Int]
        ff = f Int dEqInt

Now (f dEqInt) evaluates to a lambda that has f' as a free variable; but
f' is another thunk which evaluates to the same thing... and you end
up with a chain of identical values all hung onto by the CAF ff.

        ff = f Int dEqInt

           = let f' = f Int dEqInt in \ys. ...f'...

           = let f' = let f' = f Int dEqInt in \ys. ...f'...
                      in \ys. ...f'...

Etc.

NOTE: a bit of arity analysis would push the (f a d) inside the (\ys...),
which would make the space leak go away in this case

Solution: when typechecking the RHSs we always have in hand the
*monomorphic* Ids for each binding.  So we just need to make sure that
if (Method f a d) shows up in the constraints emerging from (...f...)
we just use the monomorphic Id.  We achieve this by adding monomorphic Ids
to the "givens" when simplifying constraints.  That's what the "lies_avail"
is doing.

Then we get

        f = /\a -> \d::Eq a -> letrec
                                 fm = \ys:[a] -> ...fm...
                               in
                               fm
-}

tcTopBinds :: [(RecFlag, LHsBinds GhcRn)] -> [LSig GhcRn]
           -> TcM (TcGblEnv, TcLclEnv)
-- The TcGblEnv contains the new tcg_binds and tcg_spects
-- The TcLclEnv has an extended type envt for the new bindings
tcTopBinds binds sigs
  = do  { -- Pattern synonym bindings populate the global environment
          (binds', (tcg_env, tcl_env)) <- tcValBinds TopLevel binds sigs $
            do { gbl <- getGblEnv
               ; lcl <- getLclEnv
               ; return (gbl, lcl) }
        ; specs <- tcImpPrags sigs   -- SPECIALISE prags for imported Ids

        ; complete_matches <- setEnvs (tcg_env, tcl_env) $ tcCompleteSigs sigs
        ; traceTc "complete_matches" (ppr binds $$ ppr sigs)
        ; traceTc "complete_matches" (ppr complete_matches)

        ; let { tcg_env' = tcg_env { tcg_imp_specs
                                      = specs ++ tcg_imp_specs tcg_env
                                   , tcg_complete_matches
                                      = complete_matches
                                          ++ tcg_complete_matches tcg_env }
                           `addTypecheckedBinds` map snd binds' }

        ; return (tcg_env', tcl_env) }
        -- The top level bindings are flattened into a giant
        -- implicitly-mutually-recursive LHsBinds

tcCompleteSigs  :: [LSig GhcRn] -> TcM [CompleteMatch]
tcCompleteSigs sigs =
  let
      doOne :: LSig GhcRn -> TcM (Maybe CompleteMatch)
      -- We don't need to "type-check" COMPLETE signatures anymore; if their
      -- combinations are invalid it will be found so at match sites.
      -- There it is also where we consider if the type of the pattern match is
      -- compatible with the result type constructor 'mb_tc'.
      doOne (L loc c@(CompleteMatchSig _ext _src_txt (L _ ns) mb_tc_nm))
        = fmap Just $ setSrcSpanA loc $ addErrCtxt (text "In" <+> ppr c) $ do
            cls   <- mkUniqDSet <$> mapM (addLocMA tcLookupConLike) ns
            mb_tc <- traverse @Maybe tcLookupLocatedTyCon mb_tc_nm
            pure CompleteMatch { cmConLikes = cls, cmResultTyCon = mb_tc }
      doOne _ = return Nothing

  -- For some reason I haven't investigated further, the signatures come in
  -- backwards wrt. declaration order. So we reverse them here, because it makes
  -- a difference for incomplete match suggestions.
  in mapMaybeM doOne $ reverse sigs

tcHsBootSigs :: [(RecFlag, LHsBinds GhcRn)] -> [LSig GhcRn] -> TcM [Id]
-- A hs-boot file has only one BindGroup, and it only has type
-- signatures in it.  The renamer checked all this
tcHsBootSigs binds sigs
  = do  { checkTc (null binds) TcRnIllegalHsBootFileDecl
        ; concatMapM (addLocMA tc_boot_sig) (filter isTypeLSig sigs) }
  where
    tc_boot_sig (TypeSig _ lnames hs_ty) = mapM f lnames
      where
        f (L _ name)
          = do { sigma_ty <- tcHsSigWcType (FunSigCtxt name NoRRC) hs_ty
               ; return (mkVanillaGlobal name sigma_ty) }
        -- Notice that we make GlobalIds, not LocalIds
    tc_boot_sig s = pprPanic "tcHsBootSigs/tc_boot_sig" (ppr s)

------------------------
tcLocalBinds :: HsLocalBinds GhcRn -> TcM thing
             -> TcM (HsLocalBinds GhcTc, thing)

tcLocalBinds (EmptyLocalBinds x) thing_inside
  = do  { thing <- thing_inside
        ; return (EmptyLocalBinds x, thing) }

tcLocalBinds (HsValBinds x (XValBindsLR (NValBinds binds sigs))) thing_inside
  = do  { (binds', thing) <- tcValBinds NotTopLevel binds sigs thing_inside
        ; return (HsValBinds x (XValBindsLR (NValBinds binds' sigs)), thing) }
tcLocalBinds (HsValBinds _ (ValBinds {})) _ = panic "tcLocalBinds"

tcLocalBinds (HsIPBinds x (IPBinds _ ip_binds)) thing_inside
  = do  { ipClass <- tcLookupClass ipClassName
        ; (given_ips, ip_binds') <-
            mapAndUnzipM (wrapLocSndMA (tc_ip_bind ipClass)) ip_binds

        -- If the binding binds ?x = E, we  must now
        -- discharge any ?x constraints in expr_lie
        -- See Note [Implicit parameter untouchables]
        ; (ev_binds, result) <- checkConstraints (IPSkol ips)
                                  [] given_ips thing_inside

        ; return (HsIPBinds x (IPBinds ev_binds ip_binds') , result) }
  where
    ips = [ip | (L _ (IPBind _ (Left (L _ ip)) _)) <- ip_binds]

        -- I wonder if we should do these one at a time
        -- Consider     ?x = 4
        --              ?y = ?x + 1
    tc_ip_bind ipClass (IPBind _ (Left (L _ ip)) expr)
       = do { ty <- newOpenFlexiTyVarTy
            ; let p = mkStrLitTy $ hsIPNameFS ip
            ; ip_id <- newDict ipClass [ p, ty ]
            ; expr' <- tcCheckMonoExpr expr ty
            ; let d = toDict ipClass p ty `fmap` expr'
            ; return (ip_id, (IPBind noAnn (Right ip_id) d)) }
    tc_ip_bind _ (IPBind _ (Right {}) _) = panic "tc_ip_bind"

    -- Coerces a `t` into a dictionary for `IP "x" t`.
    -- co : t -> IP "x" t
    toDict ipClass x ty = mkHsWrap $ mkWpCastR $
                          wrapIP $ mkClassPred ipClass [x,ty]

{- Note [Implicit parameter untouchables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We add the type variables in the types of the implicit parameters
as untouchables, not so much because we really must not unify them,
but rather because we otherwise end up with constraints like this
    Num alpha, Implic { wanted = alpha ~ Int }
The constraint solver solves alpha~Int by unification, but then
doesn't float that solved constraint out (it's not an unsolved
wanted).  Result disaster: the (Num alpha) is again solved, this
time by defaulting.  No no no.

However [Oct 10] this is all handled automatically by the
untouchable-range idea.
-}

tcValBinds :: TopLevelFlag
           -> [(RecFlag, LHsBinds GhcRn)] -> [LSig GhcRn]
           -> TcM thing
           -> TcM ([(RecFlag, LHsBinds GhcTc)], thing)

tcValBinds top_lvl binds sigs thing_inside
  = do  {   -- Typecheck the signatures
            -- It's easier to do so now, once for all the SCCs together
            -- because a single signature  f,g :: <type>
            -- might relate to more than one SCC
          (poly_ids, sig_fn) <- tcAddPatSynPlaceholders patsyns $
                                tcTySigs sigs

        -- Extend the envt right away with all the Ids
        --   declared with complete type signatures
        -- Do not extend the TcBinderStack; instead
        --   we extend it on a per-rhs basis in tcExtendForRhs
        --   See Note [Relevant bindings and the binder stack]
        --
        -- For the moment, let bindings and top-level bindings introduce
        -- only unrestricted variables.
        ; tcExtendSigIds top_lvl poly_ids $
     do { (binds', (extra_binds', thing))
              <- tcBindGroups top_lvl sig_fn prag_fn binds $
                 do { thing <- thing_inside
                       -- See Note [Pattern synonym builders don't yield dependencies]
                       --     in GHC.Rename.Bind
                    ; patsyn_builders <- mapM (tcPatSynBuilderBind prag_fn) patsyns
                    ; let extra_binds = [ (NonRecursive, builder)
                                        | builder <- patsyn_builders ]
                    ; return (extra_binds, thing) }
        ; return (binds' ++ extra_binds', thing) }}
  where
    patsyns = getPatSynBinds binds
    prag_fn = mkPragEnv sigs (foldr (unionBags . snd) emptyBag binds)

------------------------
tcBindGroups :: TopLevelFlag -> TcSigFun -> TcPragEnv
             -> [(RecFlag, LHsBinds GhcRn)] -> TcM thing
             -> TcM ([(RecFlag, LHsBinds GhcTc)], thing)
-- Typecheck a whole lot of value bindings,
-- one strongly-connected component at a time
-- Here a "strongly connected component" has the straightforward
-- meaning of a group of bindings that mention each other,
-- ignoring type signatures (that part comes later)

tcBindGroups _ _ _ [] thing_inside
  = do  { thing <- thing_inside
        ; return ([], thing) }

tcBindGroups top_lvl sig_fn prag_fn (group : groups) thing_inside
  = do  { -- See Note [Closed binder groups]
          type_env <- getLclTypeEnv
        ; let closed = isClosedBndrGroup type_env (snd group)
        ; (group', (groups', thing))
                <- tc_group top_lvl sig_fn prag_fn group closed $
                   tcBindGroups top_lvl sig_fn prag_fn groups thing_inside
        ; return (group' ++ groups', thing) }

-- Note [Closed binder groups]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
--  A mutually recursive group is "closed" if all of the free variables of
--  the bindings are closed. For example
--
-- >  h = \x -> let f = ...g...
-- >                g = ....f...x...
-- >             in ...
--
-- Here @g@ is not closed because it mentions @x@; and hence neither is @f@
-- closed.
--
-- So we need to compute closed-ness on each strongly connected components,
-- before we sub-divide it based on what type signatures it has.
--

------------------------
tc_group :: forall thing.
            TopLevelFlag -> TcSigFun -> TcPragEnv
         -> (RecFlag, LHsBinds GhcRn) -> IsGroupClosed -> TcM thing
         -> TcM ([(RecFlag, LHsBinds GhcTc)], thing)

-- Typecheck one strongly-connected component of the original program.
-- We get a list of groups back, because there may
-- be specialisations etc as well

tc_group top_lvl sig_fn prag_fn (NonRecursive, binds) closed thing_inside
        -- A single non-recursive binding
        -- We want to keep non-recursive things non-recursive
        -- so that we desugar unlifted bindings correctly
  = do { let bind = case bagToList binds of
                 [bind] -> bind
                 []     -> panic "tc_group: empty list of binds"
                 _      -> panic "tc_group: NonRecursive binds is not a singleton bag"
       ; (bind', thing) <- tc_single top_lvl sig_fn prag_fn bind closed
                                     thing_inside
       ; return ( [(NonRecursive, bind')], thing) }

tc_group top_lvl sig_fn prag_fn (Recursive, binds) closed thing_inside
  =     -- To maximise polymorphism, we do a new
        -- strongly-connected-component analysis, this time omitting
        -- any references to variables with type signatures.
        -- (This used to be optional, but isn't now.)
        -- See Note [Polymorphic recursion] in "GHC.Hs.Binds".
    do  { traceTc "tc_group rec" (pprLHsBinds binds)
        ; whenIsJust mbFirstPatSyn $ \lpat_syn ->
            recursivePatSynErr (locA $ getLoc lpat_syn) binds
        ; (binds1, thing) <- go sccs
        ; return ([(Recursive, binds1)], thing) }
                -- Rec them all together
  where
    mbFirstPatSyn = find (isPatSyn . unLoc) binds
    isPatSyn PatSynBind{} = True
    isPatSyn _ = False

    sccs :: [SCC (LHsBind GhcRn)]
    sccs = stronglyConnCompFromEdgedVerticesUniq (mkEdges sig_fn binds)

    go :: [SCC (LHsBind GhcRn)] -> TcM (LHsBinds GhcTc, thing)
    go (scc:sccs) = do  { (binds1, ids1) <- tc_scc scc
                         -- recursive bindings must be unrestricted
                         -- (the ids added to the environment here are the name of the recursive definitions).
                        ; (binds2, thing) <- tcExtendLetEnv top_lvl sig_fn closed ids1
                                                            (go sccs)
                        ; return (binds1 `unionBags` binds2, thing) }
    go []         = do  { thing <- thing_inside; return (emptyBag, thing) }

    tc_scc (AcyclicSCC bind) = tc_sub_group NonRecursive [bind]
    tc_scc (CyclicSCC binds) = tc_sub_group Recursive    binds

    tc_sub_group rec_tc binds =
      tcPolyBinds sig_fn prag_fn Recursive rec_tc closed binds

recursivePatSynErr
  :: SrcSpan -- ^ The location of the first pattern synonym binding
             --   (for error reporting)
  -> LHsBinds GhcRn
  -> TcM a
recursivePatSynErr loc binds
  = failAt loc $ TcRnRecursivePatternSynonym binds

tc_single :: forall thing.
            TopLevelFlag -> TcSigFun -> TcPragEnv
          -> LHsBind GhcRn -> IsGroupClosed -> TcM thing
          -> TcM (LHsBinds GhcTc, thing)
tc_single _top_lvl sig_fn prag_fn
          (L _ (PatSynBind _ psb@PSB{ psb_id = L _ name }))
          _ thing_inside
  = do { (aux_binds, tcg_env) <- tcPatSynDecl psb (sig_fn name) prag_fn
       ; thing <- setGblEnv tcg_env thing_inside
       ; return (aux_binds, thing)
       }

tc_single top_lvl sig_fn prag_fn lbind closed thing_inside
  = do { (binds1, ids) <- tcPolyBinds sig_fn prag_fn
                                      NonRecursive NonRecursive
                                      closed
                                      [lbind]
         -- since we are defining a non-recursive binding, it is not necessary here
         -- to define an unrestricted binding. But we do so until toplevel linear bindings are supported.
       ; thing <- tcExtendLetEnv top_lvl sig_fn closed ids thing_inside
       ; return (binds1, thing) }

------------------------
type BKey = Int -- Just number off the bindings

mkEdges :: TcSigFun -> LHsBinds GhcRn -> [Node BKey (LHsBind GhcRn)]
-- See Note [Polymorphic recursion] in "GHC.Hs.Binds".
mkEdges sig_fn binds
  = [ DigraphNode bind key [key | n <- nonDetEltsUniqSet (bind_fvs (unLoc bind)),
                         Just key <- [lookupNameEnv key_map n], no_sig n ]
    | (bind, key) <- keyd_binds
    ]
    -- It's OK to use nonDetEltsUFM here as stronglyConnCompFromEdgedVertices
    -- is still deterministic even if the edges are in nondeterministic order
    -- as explained in Note [Deterministic SCC] in GHC.Data.Graph.Directed.
  where
    bind_fvs (FunBind { fun_ext = fvs }) = fvs
    bind_fvs (PatBind { pat_ext = fvs }) = fvs
    bind_fvs _                           = emptyNameSet

    no_sig :: Name -> Bool
    no_sig n = not (hasCompleteSig sig_fn n)

    keyd_binds = bagToList binds `zip` [0::BKey ..]

    key_map :: NameEnv BKey     -- Which binding it comes from
    key_map = mkNameEnv [(bndr, key) | (L _ bind, key) <- keyd_binds
                                     , bndr <- collectHsBindBinders CollNoDictBinders bind ]

------------------------
tcPolyBinds :: TcSigFun -> TcPragEnv
            -> RecFlag         -- Whether the group is really recursive
            -> RecFlag         -- Whether it's recursive after breaking
                               -- dependencies based on type signatures
            -> IsGroupClosed   -- Whether the group is closed
            -> [LHsBind GhcRn]  -- None are PatSynBind
            -> TcM (LHsBinds GhcTc, [TcId])

-- Typechecks a single bunch of values bindings all together,
-- and generalises them.  The bunch may be only part of a recursive
-- group, because we use type signatures to maximise polymorphism
--
-- Returns a list because the input may be a single non-recursive binding,
-- in which case the dependency order of the resulting bindings is
-- important.
--
-- Knows nothing about the scope of the bindings
-- None of the bindings are pattern synonyms

tcPolyBinds sig_fn prag_fn rec_group rec_tc closed bind_list
  = setSrcSpan loc                              $
    recoverM (recoveryCode binder_names sig_fn) $ do
        -- Set up main recover; take advantage of any type sigs

    { traceTc "------------------------------------------------" Outputable.empty
    ; traceTc "Bindings for {" (ppr binder_names)
    ; dflags   <- getDynFlags
    ; let plan = decideGeneralisationPlan dflags bind_list closed sig_fn
    ; traceTc "Generalisation plan" (ppr plan)
    ; result@(_, poly_ids) <- case plan of
         NoGen              -> tcPolyNoGen rec_tc prag_fn sig_fn bind_list
         InferGen mn        -> tcPolyInfer rec_tc prag_fn sig_fn mn bind_list
         CheckGen lbind sig -> tcPolyCheck prag_fn sig lbind

    ; _concrete_evs <-
        mapM (\ poly_id ->
          hasFixedRuntimeRep (FRRBinder $ idName poly_id) (idType poly_id))
          poly_ids

    ; traceTc "} End of bindings for" (vcat [ ppr binder_names, ppr rec_group
                                            , vcat [ppr id <+> ppr (idType id) | id <- poly_ids]
                                          ])

    ; return result }
  where
    binder_names = collectHsBindListBinders CollNoDictBinders bind_list
    loc = foldr1 combineSrcSpans (map (locA . getLoc) bind_list)
         -- The mbinds have been dependency analysed and
         -- may no longer be adjacent; so find the narrowest
         -- span that includes them all

--------------
-- If typechecking the binds fails, then return with each
-- signature-less binder given type (forall a.a), to minimise
-- subsequent error messages
recoveryCode :: [Name] -> TcSigFun -> TcM (LHsBinds GhcTc, [Id])
recoveryCode binder_names sig_fn
  = do  { traceTc "tcBindsWithSigs: error recovery" (ppr binder_names)
        ; let poly_ids = map mk_dummy binder_names
        ; return (emptyBag, poly_ids) }
  where
    mk_dummy name
      | Just sig <- sig_fn name
      , Just poly_id <- completeSigPolyId_maybe sig
      = poly_id
      | otherwise
      = mkLocalId name Many forall_a_a

forall_a_a :: TcType
-- At one point I had (forall r (a :: TYPE r). a), but of course
-- that type is ill-formed: its mentions 'r' which escapes r's scope.
-- Another alternative would be (forall (a :: TYPE kappa). a), where
-- kappa is a unification variable. But I don't think we need that
-- complication here. I'm going to just use (forall (a::*). a).
-- See #15276
forall_a_a = mkSpecForAllTys [alphaTyVar] alphaTy

{- *********************************************************************
*                                                                      *
                         tcPolyNoGen
*                                                                      *
********************************************************************* -}

tcPolyNoGen     -- No generalisation whatsoever
  :: RecFlag       -- Whether it's recursive after breaking
                   -- dependencies based on type signatures
  -> TcPragEnv -> TcSigFun
  -> [LHsBind GhcRn]
  -> TcM (LHsBinds GhcTc, [TcId])

tcPolyNoGen rec_tc prag_fn tc_sig_fn bind_list
  = do { (binds', mono_infos) <- tcMonoBinds rec_tc tc_sig_fn
                                             (LetGblBndr prag_fn)
                                             bind_list
       ; mono_ids' <- mapM tc_mono_info mono_infos
       ; return (binds', mono_ids') }
  where
    tc_mono_info (MBI { mbi_poly_name = name, mbi_mono_id = mono_id })
      = do { _specs <- tcSpecPrags mono_id (lookupPragEnv prag_fn name)
           ; return mono_id }
           -- NB: tcPrags generates error messages for
           --     specialisation pragmas for non-overloaded sigs
           -- Indeed that is why we call it here!
           -- So we can safely ignore _specs


{- *********************************************************************
*                                                                      *
                         tcPolyCheck
*                                                                      *
********************************************************************* -}

tcPolyCheck :: TcPragEnv
            -> TcIdSigInfo     -- Must be a complete signature
            -> LHsBind GhcRn   -- Must be a FunBind
            -> TcM (LHsBinds GhcTc, [TcId])
-- There is just one binding,
--   it is a FunBind
--   it has a complete type signature,
tcPolyCheck prag_fn
            (CompleteSig { sig_bndr  = poly_id
                         , sig_ctxt  = ctxt
                         , sig_loc   = sig_loc })
            (L bind_loc (FunBind { fun_id = L nm_loc name
                                 , fun_matches = matches }))
  = do { traceTc "tcPolyCheck" (ppr poly_id $$ ppr sig_loc)

       ; mono_name <- newNameAt (nameOccName name) (locA nm_loc)
       ; (wrap_gen, (wrap_res, matches'))
             <- setSrcSpan sig_loc $ -- Sets the binding location for the skolems
                tcSkolemiseScoped ctxt (idType poly_id) $ \rho_ty ->
                -- Unwraps multiple layers; e.g
                --    f :: forall a. Eq a => forall b. Ord b => blah
                -- NB: tcSkolemise makes fresh type variables
                -- See Note [Instantiate sig with fresh variables]

                let mono_id = mkLocalId mono_name (varMult poly_id) rho_ty in
                tcExtendBinderStack [TcIdBndr mono_id NotTopLevel] $
                -- Why mono_id in the BinderStack?
                --    See Note [Relevant bindings and the binder stack]

                setSrcSpanA bind_loc $
                tcMatchesFun (L nm_loc mono_id) matches
                             (mkCheckExpType rho_ty)

       -- We make a funny AbsBinds, abstracting over nothing,
       -- just so we have somewhere to put the SpecPrags.
       -- Otherwise we could just use the FunBind
       -- Hence poly_id2 is just a clone of poly_id;
       -- We re-use mono-name, but we could equally well use a fresh one

       ; let prag_sigs = lookupPragEnv prag_fn name
             poly_id2  = mkLocalId mono_name (idMult poly_id) (idType poly_id)
       ; spec_prags <- tcSpecPrags    poly_id prag_sigs
       ; poly_id    <- addInlinePrags poly_id prag_sigs

       ; mod <- getModule
       ; tick <- funBindTicks (locA nm_loc) poly_id mod prag_sigs

       ; let bind' = FunBind { fun_id      = L nm_loc poly_id2
                             , fun_matches = matches'
                             , fun_ext     = wrap_gen <.> wrap_res
                             , fun_tick    = tick }

             export = ABE { abe_ext   = noExtField
                          , abe_wrap  = idHsWrapper
                          , abe_poly  = poly_id
                          , abe_mono  = poly_id2
                          , abe_prags = SpecPrags spec_prags }

             abs_bind = L bind_loc $
                        AbsBinds { abs_ext      = noExtField
                                 , abs_tvs      = []
                                 , abs_ev_vars  = []
                                 , abs_ev_binds = []
                                 , abs_exports  = [export]
                                 , abs_binds    = unitBag (L bind_loc bind')
                                 , abs_sig      = True }

       ; return (unitBag abs_bind, [poly_id]) }

tcPolyCheck _prag_fn sig bind
  = pprPanic "tcPolyCheck" (ppr sig $$ ppr bind)

funBindTicks :: SrcSpan -> TcId -> Module -> [LSig GhcRn]
             -> TcM [CoreTickish]
funBindTicks loc fun_id mod sigs
  | (mb_cc_str : _) <- [ cc_name | L _ (SCCFunSig _ _ _ cc_name) <- sigs ]
      -- this can only be a singleton list, as duplicate pragmas are rejected
      -- by the renamer
  , let cc_str
          | Just cc_str <- mb_cc_str
          = sl_fs $ unLoc cc_str
          | otherwise
          = getOccFS (Var.varName fun_id)
        cc_name = moduleNameFS (moduleName mod) `appendFS` consFS '.' cc_str
  = do
      flavour <- DeclCC <$> getCCIndexTcM cc_name
      let cc = mkUserCC cc_name mod loc flavour
      return [ProfNote cc True True]
  | otherwise
  = return []

{- Note [Instantiate sig with fresh variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's vital to instantiate a type signature with fresh variables.
For example:
      type T = forall a. [a] -> [a]
      f :: T;
      f = g where { g :: T; g = <rhs> }

 We must not use the same 'a' from the defn of T at both places!!
(Instantiation is only necessary because of type synonyms.  Otherwise,
it's all cool; each signature has distinct type variables from the renamer.)
-}


{- *********************************************************************
*                                                                      *
                         tcPolyInfer
*                                                                      *
********************************************************************* -}

tcPolyInfer
  :: RecFlag       -- Whether it's recursive after breaking
                   -- dependencies based on type signatures
  -> TcPragEnv -> TcSigFun
  -> Bool         -- True <=> apply the monomorphism restriction
  -> [LHsBind GhcRn]
  -> TcM (LHsBinds GhcTc, [TcId])
tcPolyInfer rec_tc prag_fn tc_sig_fn mono bind_list
  = do { (tclvl, wanted, (binds', mono_infos))
             <- pushLevelAndCaptureConstraints  $
                tcMonoBinds rec_tc tc_sig_fn LetLclBndr bind_list

       ; let name_taus  = [ (mbi_poly_name info, idType (mbi_mono_id info))
                          | info <- mono_infos ]
             sigs       = [ sig | MBI { mbi_sig = Just sig } <- mono_infos ]
             infer_mode = if mono then ApplyMR else NoRestrictions

       ; mapM_ (checkOverloadedSig mono) sigs

       ; traceTc "simplifyInfer call" (ppr tclvl $$ ppr name_taus $$ ppr wanted)
       ; (qtvs, givens, ev_binds, insoluble)
                 <- simplifyInfer tclvl infer_mode sigs name_taus wanted

       ; let inferred_theta = map evVarPred givens
       ; exports <- checkNoErrs $
                    mapM (mkExport prag_fn insoluble qtvs inferred_theta) mono_infos

       ; loc <- getSrcSpanM
       ; let poly_ids = map abe_poly exports
             abs_bind = L (noAnnSrcSpan loc) $
                        AbsBinds { abs_ext = noExtField
                                 , abs_tvs = qtvs
                                 , abs_ev_vars = givens, abs_ev_binds = [ev_binds]
                                 , abs_exports = exports, abs_binds = binds'
                                 , abs_sig = False }

       ; traceTc "Binding:" (ppr (poly_ids `zip` map idType poly_ids))
       ; return (unitBag abs_bind, poly_ids) }
         -- poly_ids are guaranteed zonked by mkExport

--------------
mkExport :: TcPragEnv
         -> Bool                        -- True <=> there was an insoluble type error
                                        --          when typechecking the bindings
         -> [TyVar] -> TcThetaType      -- Both already zonked
         -> MonoBindInfo
         -> TcM (ABExport GhcTc)
-- Only called for generalisation plan InferGen, not by CheckGen or NoGen
--
-- mkExport generates exports with
--      zonked type variables,
--      zonked poly_ids
-- The former is just because no further unifications will change
-- the quantified type variables, so we can fix their final form
-- right now.
-- The latter is needed because the poly_ids are used to extend the
-- type environment; see the invariant on GHC.Tc.Utils.Env.tcExtendIdEnv

-- Pre-condition: the qtvs and theta are already zonked

mkExport prag_fn insoluble qtvs theta
         mono_info@(MBI { mbi_poly_name = poly_name
                        , mbi_sig       = mb_sig
                        , mbi_mono_id   = mono_id })
  = do  { mono_ty <- zonkTcType (idType mono_id)
        ; poly_id <- mkInferredPolyId insoluble qtvs theta poly_name mb_sig mono_ty

        -- NB: poly_id has a zonked type
        ; poly_id <- addInlinePrags poly_id prag_sigs
        ; spec_prags <- tcSpecPrags poly_id prag_sigs
                -- tcPrags requires a zonked poly_id

        -- See Note [Impedance matching]
        -- NB: we have already done checkValidType, including an ambiguity check,
        --     on the type; either when we checked the sig or in mkInferredPolyId
        ; let poly_ty     = idType poly_id
              sel_poly_ty = mkInfSigmaTy qtvs theta mono_ty
                -- This type is just going into tcSubType,
                -- so Inferred vs. Specified doesn't matter

        ; wrap <- if sel_poly_ty `eqType` poly_ty  -- NB: eqType ignores visibility
                  then return idHsWrapper  -- Fast path; also avoids complaint when we infer
                                           -- an ambiguous type and have AllowAmbiguousType
                                           -- e..g infer  x :: forall a. F a -> Int
                  else addErrCtxtM (mk_impedance_match_msg mono_info sel_poly_ty poly_ty) $
                       tcSubTypeSigma sig_ctxt sel_poly_ty poly_ty

        ; localSigWarn poly_id mb_sig

        ; return (ABE { abe_ext = noExtField
                      , abe_wrap = wrap
                        -- abe_wrap :: idType poly_id ~ (forall qtvs. theta => mono_ty)
                      , abe_poly  = poly_id
                      , abe_mono  = mono_id
                      , abe_prags = SpecPrags spec_prags }) }
  where
    prag_sigs = lookupPragEnv prag_fn poly_name
    sig_ctxt  = InfSigCtxt poly_name

mkInferredPolyId :: Bool  -- True <=> there was an insoluble error when
                          --          checking the binding group for this Id
                 -> [TyVar] -> TcThetaType
                 -> Name -> Maybe TcIdSigInst -> TcType
                 -> TcM TcId
mkInferredPolyId insoluble qtvs inferred_theta poly_name mb_sig_inst mono_ty
  | Just (TISI { sig_inst_sig = sig })  <- mb_sig_inst
  , CompleteSig { sig_bndr = poly_id } <- sig
  = return poly_id

  | otherwise  -- Either no type sig or partial type sig
  = checkNoErrs $  -- The checkNoErrs ensures that if the type is ambiguous
                   -- we don't carry on to the impedance matching, and generate
                   -- a duplicate ambiguity error.  There is a similar
                   -- checkNoErrs for complete type signatures too.
    do { fam_envs <- tcGetFamInstEnvs
       ; let mono_ty' = reductionReducedType $ normaliseType fam_envs Nominal mono_ty
               -- Unification may not have normalised the type,
               -- so do it here to make it as uncomplicated as possible.
               -- Example: f :: [F Int] -> Bool
               -- should be rewritten to f :: [Char] -> Bool, if possible
               --
               -- We can discard the coercion _co, because we'll reconstruct
               -- it in the call to tcSubType below

       ; (binders, theta') <- chooseInferredQuantifiers inferred_theta
                                (tyCoVarsOfType mono_ty') qtvs mb_sig_inst

       ; let inferred_poly_ty = mkInvisForAllTys binders (mkPhiTy theta' mono_ty')

       ; traceTc "mkInferredPolyId" (vcat [ppr poly_name, ppr qtvs, ppr theta'
                                          , ppr inferred_poly_ty])
       ; unless insoluble $
         addErrCtxtM (mk_inf_msg poly_name inferred_poly_ty) $
         checkValidType (InfSigCtxt poly_name) inferred_poly_ty
         -- See Note [Validity of inferred types]
         -- If we found an insoluble error in the function definition, don't
         -- do this check; otherwise (#14000) we may report an ambiguity
         -- error for a rather bogus type.

       ; return (mkLocalId poly_name Many inferred_poly_ty) }


chooseInferredQuantifiers :: TcThetaType   -- inferred
                          -> TcTyVarSet    -- tvs free in tau type
                          -> [TcTyVar]     -- inferred quantified tvs
                          -> Maybe TcIdSigInst
                          -> TcM ([InvisTVBinder], TcThetaType)
chooseInferredQuantifiers inferred_theta tau_tvs qtvs Nothing
  = -- No type signature (partial or complete) for this binder,
    do { let free_tvs = closeOverKinds (growThetaTyVars inferred_theta tau_tvs)
                        -- Include kind variables!  #7916
             my_theta = pickCapturedPreds free_tvs inferred_theta
             binders  = [ mkTyVarBinder InferredSpec tv
                        | tv <- qtvs
                        , tv `elemVarSet` free_tvs ]
       ; return (binders, my_theta) }

chooseInferredQuantifiers inferred_theta tau_tvs qtvs
                          (Just (TISI { sig_inst_sig   = sig  -- Always PartialSig
                                      , sig_inst_wcx   = wcx
                                      , sig_inst_theta = annotated_theta
                                      , sig_inst_skols = annotated_tvs }))
  = -- Choose quantifiers for a partial type signature
    do { let (psig_qtv_nms, psig_qtv_bndrs) = unzip annotated_tvs
       ; psig_qtv_bndrs <- mapM zonkInvisTVBinder psig_qtv_bndrs
       ; let psig_qtvs    = map binderVar psig_qtv_bndrs
             psig_qtv_set = mkVarSet psig_qtvs
             psig_qtv_prs = psig_qtv_nms `zip` psig_qtvs


            -- Check whether the quantified variables of the
            -- partial signature have been unified together
            -- See Note [Quantified variables in partial type signatures]
       ; mapM_ report_dup_tyvar_tv_err  (findDupTyVarTvs psig_qtv_prs)

            -- Check whether a quantified variable of the partial type
            -- signature is not actually quantified.  How can that happen?
            -- See Note [Quantification and partial signatures] Wrinkle 4
            --     in GHC.Tc.Solver
       ; mapM_ report_mono_sig_tv_err [ n | (n,tv) <- psig_qtv_prs
                                          , not (tv `elem` qtvs) ]

       ; annotated_theta      <- zonkTcTypes annotated_theta
       ; (free_tvs, my_theta) <- choose_psig_context psig_qtv_set annotated_theta wcx

       ; let keep_me    = free_tvs `unionVarSet` psig_qtv_set
             final_qtvs = [ mkTyVarBinder vis tv
                          | tv <- qtvs -- Pulling from qtvs maintains original order
                          , tv `elemVarSet` keep_me
                          , let vis = case lookupVarBndr tv psig_qtv_bndrs of
                                  Just spec -> spec
                                  Nothing   -> InferredSpec ]

       ; return (final_qtvs, my_theta) }
  where
    report_dup_tyvar_tv_err (n1,n2)
      | PartialSig { psig_name = fn_name, psig_hs_ty = hs_ty } <- sig
      = addErrTc (TcRnPartialTypeSigTyVarMismatch n1 n2 fn_name hs_ty)
      | otherwise -- Can't happen; by now we know it's a partial sig
      = pprPanic "report_tyvar_tv_err" (ppr sig)

    report_mono_sig_tv_err n
      | PartialSig { psig_name = fn_name, psig_hs_ty = hs_ty } <- sig
      = addErrTc (TcRnPartialTypeSigBadQuantifier n fn_name hs_ty)
      | otherwise -- Can't happen; by now we know it's a partial sig
      = pprPanic "report_mono_sig_tv_err" (ppr sig)

    choose_psig_context :: VarSet -> TcThetaType -> Maybe TcType
                        -> TcM (VarSet, TcThetaType)
    choose_psig_context _ annotated_theta Nothing
      = do { let free_tvs = closeOverKinds (tyCoVarsOfTypes annotated_theta
                                            `unionVarSet` tau_tvs)
           ; return (free_tvs, annotated_theta) }

    choose_psig_context psig_qtvs annotated_theta (Just wc_var_ty)
      = do { let free_tvs = closeOverKinds (growThetaTyVars inferred_theta seed_tvs)
                            -- growThetaVars just like the no-type-sig case
                            -- Omitting this caused #12844
                 seed_tvs = tyCoVarsOfTypes annotated_theta  -- These are put there
                            `unionVarSet` tau_tvs            --       by the user

           ; let keep_me  = psig_qtvs `unionVarSet` free_tvs
                 my_theta = pickCapturedPreds keep_me inferred_theta

           -- Fill in the extra-constraints wildcard hole with inferred_theta,
           -- so that the Hole constraint we have already emitted
           -- (in tcHsPartialSigType) can report what filled it in.
           -- NB: my_theta already includes all the annotated constraints
           ; diff_theta <- findInferredDiff annotated_theta my_theta

           ; case tcGetCastedTyVar_maybe wc_var_ty of
               -- We know that wc_co must have type kind(wc_var) ~ Constraint, as it
               -- comes from the checkExpectedKind in GHC.Tc.Gen.HsType.tcAnonWildCardOcc.
               -- So, to make the kinds work out, we reverse the cast here.
               Just (wc_var, wc_co) -> writeMetaTyVar wc_var (mk_ctuple diff_theta
                                                              `mkCastTy` mkTcSymCo wc_co)
               Nothing              -> pprPanic "chooseInferredQuantifiers 1" (ppr wc_var_ty)

           ; traceTc "completeTheta" $
                vcat [ ppr sig
                     , text "annotated_theta:" <+> ppr annotated_theta
                     , text "inferred_theta:" <+> ppr inferred_theta
                     , text "my_theta:" <+> ppr my_theta
                     , text "diff_theta:" <+> ppr diff_theta ]
           ; return (free_tvs, annotated_theta ++ diff_theta) }
             -- Return (annotated_theta ++ diff_theta)
             -- See Note [Extra-constraints wildcards]

    mk_ctuple preds = mkBoxedTupleTy preds
       -- Hack alert!  See GHC.Tc.Gen.HsType:
       -- Note [Extra-constraint holes in partial type signatures]

mk_impedance_match_msg :: MonoBindInfo
                       -> TcType -> TcType
                       -> TidyEnv -> TcM (TidyEnv, SDoc)
-- This is a rare but rather awkward error messages
mk_impedance_match_msg (MBI { mbi_poly_name = name, mbi_sig = mb_sig })
                       inf_ty sig_ty tidy_env
 = do { (tidy_env1, inf_ty) <- zonkTidyTcType tidy_env  inf_ty
      ; (tidy_env2, sig_ty) <- zonkTidyTcType tidy_env1 sig_ty
      ; let msg = vcat [ text "When checking that the inferred type"
                       , nest 2 $ ppr name <+> dcolon <+> ppr inf_ty
                       , text "is as general as its" <+> what <+> text "signature"
                       , nest 2 $ ppr name <+> dcolon <+> ppr sig_ty ]
      ; return (tidy_env2, msg) }
  where
    what = case mb_sig of
             Nothing                     -> text "inferred"
             Just sig | isPartialSig sig -> text "(partial)"
                      | otherwise        -> empty


mk_inf_msg :: Name -> TcType -> TidyEnv -> TcM (TidyEnv, SDoc)
mk_inf_msg poly_name poly_ty tidy_env
 = do { (tidy_env1, poly_ty) <- zonkTidyTcType tidy_env poly_ty
      ; let msg = vcat [ text "When checking the inferred type"
                       , nest 2 $ ppr poly_name <+> dcolon <+> ppr poly_ty ]
      ; return (tidy_env1, msg) }


-- | Warn the user about polymorphic local binders that lack type signatures.
localSigWarn :: Id -> Maybe TcIdSigInst -> TcM ()
localSigWarn id mb_sig
  | Just _ <- mb_sig               = return ()
  | not (isSigmaTy (idType id))    = return ()
  | otherwise                      = warnMissingSignatures id

warnMissingSignatures :: Id -> TcM ()
warnMissingSignatures id
  = do  { env0 <- tcInitTidyEnv
        ; let (env1, tidy_ty) = tidyOpenType env0 (idType id)
        ; let dia = TcRnPolymorphicBinderMissingSig (idName id) tidy_ty
        ; addDiagnosticTcM (env1, dia) }

checkOverloadedSig :: Bool -> TcIdSigInst -> TcM ()
-- Example:
--   f :: Eq a => a -> a
--   K f = e
-- The MR applies, but the signature is overloaded, and it's
-- best to complain about this directly
-- c.f #11339
checkOverloadedSig monomorphism_restriction_applies sig
  | not (null (sig_inst_theta sig))
  , monomorphism_restriction_applies
  , let orig_sig = sig_inst_sig sig
  = setSrcSpan (sig_loc orig_sig) $
    failWith $ TcRnOverloadedSig orig_sig
  | otherwise
  = return ()

{- Note [Partial type signatures and generalisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If /any/ of the signatures in the group is a partial type signature
   f :: _ -> Int
then we *always* use the InferGen plan, and hence tcPolyInfer.
We do this even for a local binding with -XMonoLocalBinds, when
we normally use NoGen.

Reasons:
  * The TcSigInfo for 'f' has a unification variable for the '_',
    whose TcLevel is one level deeper than the current level.
    (See pushTcLevelM in tcTySig.)  But NoGen doesn't increase
    the TcLevel like InferGen, so we lose the level invariant.

  * The signature might be   f :: forall a. _ -> a
    so it really is polymorphic.  It's not clear what it would
    mean to use NoGen on this, and indeed the ASSERT in tcLhs,
    in the (Just sig) case, checks that if there is a signature
    then we are using LetLclBndr, and hence a nested AbsBinds with
    increased TcLevel

It might be possible to fix these difficulties somehow, but there
doesn't seem much point.  Indeed, adding a partial type signature is a
way to get per-binding inferred generalisation.

We apply the MR if /all/ of the partial signatures lack a context.
In particular (#11016):
   f2 :: (?loc :: Int) => _
   f2 = ?loc
It's stupid to apply the MR here.  This test includes an extra-constraints
wildcard; that is, we don't apply the MR if you write
   f3 :: _ => blah

Note [Quantified variables in partial type signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  f :: forall a. a -> a -> _
  f x y = g x y
  g :: forall b. b -> b -> _
  g x y = [x, y]

Here, 'f' and 'g' are mutually recursive, and we end up unifying 'a' and 'b'
together, which is fine.  So we bind 'a' and 'b' to TyVarTvs, which can then
unify with each other.

But now consider:
  f :: forall a b. a -> b -> _
  f x y = [x, y]

We want to get an error from this, because 'a' and 'b' get unified.
So we make a test, one per partial signature, to check that the
explicitly-quantified type variables have not been unified together.
#14449 showed this up.

Note [Extra-constraints wildcards]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this from #18646
    class Foo x where
      foo :: x

    bar :: (Foo (), _) => f ()
    bar = pure foo

We get [W] Foo (), [W] Applicative f.   When we do pickCapturedPreds in
choose_psig_context, we'll discard Foo ()!  Usually would not quantify over
such (closed) predicates.  So my_theta will be (Applicative f). But we really
do want to quantify over (Foo ()) -- it was speicfied by the programmer.
Solution: always return annotated_theta (user-specified) plus the extra piece
diff_theta.

Note [Validity of inferred types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We need to check inferred type for validity, in case it uses language
extensions that are not turned on.  The principle is that if the user
simply adds the inferred type to the program source, it'll compile fine.
See #8883.

Examples that might fail:
 - the type might be ambiguous

 - an inferred theta that requires type equalities e.g. (F a ~ G b)
                                or multi-parameter type classes
 - an inferred type that includes unboxed tuples


Note [Impedance matching]
~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   f 0 x = x
   f n x = g [] (not x)

   g [] y = f 10 y
   g _  y = f 9  y

After typechecking we'll get
  f_mono_ty :: a -> Bool -> Bool
  g_mono_ty :: [b] -> Bool -> Bool
with constraints
  (Eq a, Num a)

Note that f is polymorphic in 'a' and g in 'b'; and these are not linked.
The types we really want for f and g are
   f :: forall a. (Eq a, Num a) => a -> Bool -> Bool
   g :: forall b. [b] -> Bool -> Bool

We can get these by "impedance matching":
   tuple :: forall a b. (Eq a, Num a) => (a -> Bool -> Bool, [b] -> Bool -> Bool)
   tuple a b d1 d1 = let ...bind f_mono, g_mono in (f_mono, g_mono)

   f a d1 d2 = case tuple a Any d1 d2 of (f, g) -> f
   g b = case tuple Integer b dEqInteger dNumInteger of (f,g) -> g

Suppose the shared quantified tyvars are qtvs and constraints theta.
Then we want to check that
     forall qtvs. theta => f_mono_ty   is more polymorphic than   f's polytype
and the proof is the impedance matcher.

Notice that the impedance matcher may do defaulting.  See #7173.

It also cleverly does an ambiguity check; for example, rejecting
   f :: F a -> F a
where F is a non-injective type function.
-}


{-
Note [SPECIALISE pragmas]
~~~~~~~~~~~~~~~~~~~~~~~~~
There is no point in a SPECIALISE pragma for a non-overloaded function:
   reverse :: [a] -> [a]
   {-# SPECIALISE reverse :: [Int] -> [Int] #-}

But SPECIALISE INLINE *can* make sense for GADTS:
   data Arr e where
     ArrInt :: !Int -> ByteArray# -> Arr Int
     ArrPair :: !Int -> Arr e1 -> Arr e2 -> Arr (e1, e2)

   (!:) :: Arr e -> Int -> e
   {-# SPECIALISE INLINE (!:) :: Arr Int -> Int -> Int #-}
   {-# SPECIALISE INLINE (!:) :: Arr (a, b) -> Int -> (a, b) #-}
   (ArrInt _ ba)     !: (I# i) = I# (indexIntArray# ba i)
   (ArrPair _ a1 a2) !: i      = (a1 !: i, a2 !: i)

When (!:) is specialised it becomes non-recursive, and can usefully
be inlined.  Scary!  So we only warn for SPECIALISE *without* INLINE
for a non-overloaded function.

************************************************************************
*                                                                      *
                         tcMonoBinds
*                                                                      *
************************************************************************

@tcMonoBinds@ deals with a perhaps-recursive group of HsBinds.
The signatures have been dealt with already.
-}

data MonoBindInfo = MBI { mbi_poly_name :: Name
                        , mbi_sig       :: Maybe TcIdSigInst
                        , mbi_mono_id   :: TcId }

tcMonoBinds :: RecFlag  -- Whether the binding is recursive for typechecking purposes
                        -- i.e. the binders are mentioned in their RHSs, and
                        --      we are not rescued by a type signature
            -> TcSigFun -> LetBndrSpec
            -> [LHsBind GhcRn]
            -> TcM (LHsBinds GhcTc, [MonoBindInfo])

-- SPECIAL CASE 1: see Note [Special case for non-recursive function bindings]
tcMonoBinds is_rec sig_fn no_gen
           [ L b_loc (FunBind { fun_id = L nm_loc name
                              , fun_matches = matches })]
                             -- Single function binding,
  | NonRecursive <- is_rec   -- ...binder isn't mentioned in RHS
  , Nothing <- sig_fn name   -- ...with no type signature
  = setSrcSpanA b_loc    $
    do  { ((co_fn, matches'), mono_id, _) <- fixM $ \ ~(_, _, rhs_ty) ->
                                          -- See Note [fixM for rhs_ty in tcMonoBinds]
            do  { mono_id <- newLetBndr no_gen name Many rhs_ty
                ; (matches', rhs_ty')
                    <- tcInfer $ \ exp_ty ->
                       tcExtendBinderStack [TcIdBndr_ExpType name exp_ty NotTopLevel] $
                          -- We extend the error context even for a non-recursive
                          -- function so that in type error messages we show the
                          -- type of the thing whose rhs we are type checking
                       tcMatchesFun (L nm_loc mono_id) matches exp_ty
                ; return (matches', mono_id, rhs_ty')
                }

        ; return (unitBag $ L b_loc $
                     FunBind { fun_id = L nm_loc mono_id,
                               fun_matches = matches',
                               fun_ext = co_fn, fun_tick = [] },
                  [MBI { mbi_poly_name = name
                       , mbi_sig       = Nothing
                       , mbi_mono_id   = mono_id }]) }

-- SPECIAL CASE 2: see Note [Special case for non-recursive pattern bindings]
tcMonoBinds is_rec sig_fn no_gen
           [L b_loc (PatBind { pat_lhs = pat, pat_rhs = grhss })]
  | NonRecursive <- is_rec   -- ...binder isn't mentioned in RHS
  , all (isNothing . sig_fn) bndrs
  = addErrCtxt (patMonoBindsCtxt pat grhss) $
    do { (grhss', pat_ty) <- tcInfer $ \ exp_ty ->
                             tcGRHSsPat grhss exp_ty

       ; let exp_pat_ty :: Scaled ExpSigmaType
             exp_pat_ty = unrestricted (mkCheckExpType pat_ty)
       ; (pat', mbis) <- tcLetPat (const Nothing) no_gen pat exp_pat_ty $
                         mapM lookupMBI bndrs

       ; return ( unitBag $ L b_loc $
                     PatBind { pat_lhs = pat', pat_rhs = grhss'
                             , pat_ext = pat_ty, pat_ticks = ([],[]) }

                , mbis ) }
  where
    bndrs = collectPatBinders CollNoDictBinders pat

-- GENERAL CASE
tcMonoBinds _ sig_fn no_gen binds
  = do  { tc_binds <- mapM (wrapLocMA (tcLhs sig_fn no_gen)) binds

        -- Bring the monomorphic Ids, into scope for the RHSs
        ; let mono_infos = getMonoBindInfo tc_binds
              rhs_id_env = [ (name, mono_id)
                           | MBI { mbi_poly_name = name
                                 , mbi_sig       = mb_sig
                                 , mbi_mono_id   = mono_id } <- mono_infos
                           , case mb_sig of
                               Just sig -> isPartialSig sig
                               Nothing  -> True ]
                -- A monomorphic binding for each term variable that lacks
                -- a complete type sig.  (Ones with a sig are already in scope.)

        ; traceTc "tcMonoBinds" $ vcat [ ppr n <+> ppr id <+> ppr (idType id)
                                       | (n,id) <- rhs_id_env]
        ; binds' <- tcExtendRecIds rhs_id_env $
                    mapM (wrapLocMA tcRhs) tc_binds

        ; return (listToBag binds', mono_infos) }

{- Note [Special case for non-recursive function bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the special case of
* A non-recursive FunBind
* With no type signature
we infer the type of the right hand side first (it may have a
higher-rank type) and *then* make the monomorphic Id for the LHS e.g.
   f = \(x::forall a. a->a) -> <body>

We want to infer a higher-rank type for f

Note [Special case for non-recursive pattern bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the special case of
* A pattern binding
* With no type signature for any of the binders
we can /infer/ the type of the RHS, and /check/ the pattern
against that type.  For example (#18323)

  ids :: [forall a. a -> a]
  combine :: (forall a . [a] -> a) -> [forall a. a -> a]
          -> ((forall a . [a] -> a), [forall a. a -> a])

  (x,y) = combine head ids

with -XImpredicativeTypes we can infer a good type for
(combine head ids), and use that to tell us the polymorphic
types of x and y.

We don't need to check -XImpredicativeTypes beucase without it
these types like [forall a. a->a] are illegal anyway, so this
special case code only really has an effect if -XImpredicativeTypes
is on.  Small exception:
  (x) = e
is currently treated as a pattern binding so, even absent
-XImpredicativeTypes, we will get a small improvement in behaviour.
But I don't think it's worth an extension flag.

Why do we require no type signatures on /any/ of the binders?
Consider
   x :: forall a. a->a
   y :: forall a. a->a
   (x,y) = (id,id)

Here we should /check/ the RHS with expected type
  (forall a. a->a, forall a. a->a).

If we have no signatures, we can the approach of this Note
to /infer/ the type of the RHS.

But what if we have some signatures, but not all? Say this:
  p :: forall a. a->a
  (p,q) = (id,  (\(x::forall b. b->b). x True))

Here we want to push p's signature inwards, i.e. /checking/, to
correctly elaborate 'id'. But we want to /infer/ q's higher rank
type.  There seems to be no way to do this.  So currently we only
switch to inference when we have no signature for any of the binders.

Note [fixM for rhs_ty in tcMonoBinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In order to create mono_id we need rhs_ty but we don't have it yet,
we only get it from tcMatchesFun later (which needs mono_id to put
into HsMatchContext for pretty printing). To solve this, create
a thunk of rhs_ty with fixM that we fill in later.

This is fine only because neither newLetBndr or tcMatchesFun look
at the varType field of the Id. tcMatchesFun only looks at idName
of mono_id.

Also see #20415 for the bigger picture of why tcMatchesFun needs
mono_id in the first place.
-}


------------------------
-- tcLhs typechecks the LHS of the bindings, to construct the environment in which
-- we typecheck the RHSs.  Basically what we are doing is this: for each binder:
--      if there's a signature for it, use the instantiated signature type
--      otherwise invent a type variable
-- You see that quite directly in the FunBind case.
--
-- But there's a complication for pattern bindings:
--      data T = MkT (forall a. a->a)
--      MkT f = e
-- Here we can guess a type variable for the entire LHS (which will be refined to T)
-- but we want to get (f::forall a. a->a) as the RHS environment.
-- The simplest way to do this is to typecheck the pattern, and then look up the
-- bound mono-ids.  Then we want to retain the typechecked pattern to avoid re-doing
-- it; hence the TcMonoBind data type in which the LHS is done but the RHS isn't

data TcMonoBind         -- Half completed; LHS done, RHS not done
  = TcFunBind  MonoBindInfo  SrcSpan (MatchGroup GhcRn (LHsExpr GhcRn))
  | TcPatBind [MonoBindInfo] (LPat GhcTc) (GRHSs GhcRn (LHsExpr GhcRn))
              TcSigmaType

tcLhs :: TcSigFun -> LetBndrSpec -> HsBind GhcRn -> TcM TcMonoBind
-- Only called with plan InferGen (LetBndrSpec = LetLclBndr)
--                    or NoGen    (LetBndrSpec = LetGblBndr)
-- CheckGen is used only for functions with a complete type signature,
--          and tcPolyCheck doesn't use tcMonoBinds at all

tcLhs sig_fn no_gen (FunBind { fun_id = L nm_loc name
                             , fun_matches = matches })
  | Just (TcIdSig sig) <- sig_fn name
  = -- There is a type signature.
    -- It must be partial; if complete we'd be in tcPolyCheck!
    --    e.g.   f :: _ -> _
    --           f x = ...g...
    --           Just g = ...f...
    -- Hence always typechecked with InferGen
    do { mono_info <- tcLhsSigId no_gen (name, sig)
       ; return (TcFunBind mono_info (locA nm_loc) matches) }

  | otherwise  -- No type signature
  = do { mono_ty <- newOpenFlexiTyVarTy
       ; mono_id <- newLetBndr no_gen name Many mono_ty
          -- This ^ generates a binder with Many multiplicity because all
          -- let/where-binders are unrestricted. When we introduce linear let
          -- binders, we will need to retrieve the multiplicity information.
       ; let mono_info = MBI { mbi_poly_name = name
                             , mbi_sig       = Nothing
                             , mbi_mono_id   = mono_id }
       ; return (TcFunBind mono_info (locA nm_loc) matches) }

tcLhs sig_fn no_gen (PatBind { pat_lhs = pat, pat_rhs = grhss })
  = -- See Note [Typechecking pattern bindings]
    do  { sig_mbis <- mapM (tcLhsSigId no_gen) sig_names

        ; let inst_sig_fun = lookupNameEnv $ mkNameEnv $
                             [ (mbi_poly_name mbi, mbi_mono_id mbi)
                             | mbi <- sig_mbis ]

            -- See Note [Existentials in pattern bindings]
        ; ((pat', nosig_mbis), pat_ty)
            <- addErrCtxt (patMonoBindsCtxt pat grhss) $
               tcInfer $ \ exp_ty ->
               tcLetPat inst_sig_fun no_gen pat (unrestricted exp_ty) $
                 -- The above inferred type get an unrestricted multiplicity. It may be
                 -- worth it to try and find a finer-grained multiplicity here
                 -- if examples warrant it.
               mapM lookupMBI nosig_names

        ; let mbis = sig_mbis ++ nosig_mbis

        ; traceTc "tcLhs" (vcat [ ppr id <+> dcolon <+> ppr (idType id)
                                | mbi <- mbis, let id = mbi_mono_id mbi ]
                           $$ ppr no_gen)

        ; return (TcPatBind mbis pat' grhss pat_ty) }
  where
    bndr_names = collectPatBinders CollNoDictBinders pat
    (nosig_names, sig_names) = partitionWith find_sig bndr_names

    find_sig :: Name -> Either Name (Name, TcIdSigInfo)
    find_sig name = case sig_fn name of
                      Just (TcIdSig sig) -> Right (name, sig)
                      _                  -> Left name

tcLhs _ _ other_bind = pprPanic "tcLhs" (ppr other_bind)
        -- AbsBind, VarBind impossible

lookupMBI :: Name -> TcM MonoBindInfo
-- After typechecking the pattern, look up the binder
-- names that lack a signature, which the pattern has brought
-- into scope.
lookupMBI name
  = do { mono_id <- tcLookupId name
       ; return (MBI { mbi_poly_name = name
                     , mbi_sig       = Nothing
                     , mbi_mono_id   = mono_id }) }

-------------------
tcLhsSigId :: LetBndrSpec -> (Name, TcIdSigInfo) -> TcM MonoBindInfo
tcLhsSigId no_gen (name, sig)
  = do { inst_sig <- tcInstSig sig
       ; mono_id <- newSigLetBndr no_gen name inst_sig
       ; return (MBI { mbi_poly_name = name
                     , mbi_sig       = Just inst_sig
                     , mbi_mono_id   = mono_id }) }

------------
newSigLetBndr :: LetBndrSpec -> Name -> TcIdSigInst -> TcM TcId
newSigLetBndr (LetGblBndr prags) name (TISI { sig_inst_sig = id_sig })
  | CompleteSig { sig_bndr = poly_id } <- id_sig
  = addInlinePrags poly_id (lookupPragEnv prags name)
newSigLetBndr no_gen name (TISI { sig_inst_tau = tau })
  = newLetBndr no_gen name Many tau
    -- Binders with a signature are currently always of multiplicity
    -- Many. Because they come either from toplevel, let, or where
    -- declarations. Which are all unrestricted currently.

-------------------
tcRhs :: TcMonoBind -> TcM (HsBind GhcTc)
tcRhs (TcFunBind info@(MBI { mbi_sig = mb_sig, mbi_mono_id = mono_id })
                 loc matches)
  = tcExtendIdBinderStackForRhs [info]  $
    tcExtendTyVarEnvForRhs mb_sig       $
    do  { traceTc "tcRhs: fun bind" (ppr mono_id $$ ppr (idType mono_id))
        ; (co_fn, matches') <- tcMatchesFun (L (noAnnSrcSpan loc) mono_id)
                                 matches (mkCheckExpType $ idType mono_id)
        ; return ( FunBind { fun_id = L (noAnnSrcSpan loc) mono_id
                           , fun_matches = matches'
                           , fun_ext = co_fn
                           , fun_tick = [] } ) }

tcRhs (TcPatBind infos pat' grhss pat_ty)
  = -- When we are doing pattern bindings we *don't* bring any scoped
    -- type variables into scope unlike function bindings
    -- Wny not?  They are not completely rigid.
    -- That's why we have the special case for a single FunBind in tcMonoBinds
    tcExtendIdBinderStackForRhs infos        $
    do  { traceTc "tcRhs: pat bind" (ppr pat' $$ ppr pat_ty)
        ; grhss' <- addErrCtxt (patMonoBindsCtxt pat' grhss) $
                    tcGRHSsPat grhss (mkCheckExpType pat_ty)

        ; return ( PatBind { pat_lhs = pat', pat_rhs = grhss'
                           , pat_ext = pat_ty
                           , pat_ticks = ([],[]) } )}

tcExtendTyVarEnvForRhs :: Maybe TcIdSigInst -> TcM a -> TcM a
tcExtendTyVarEnvForRhs Nothing thing_inside
  = thing_inside
tcExtendTyVarEnvForRhs (Just sig) thing_inside
  = tcExtendTyVarEnvFromSig sig thing_inside

tcExtendTyVarEnvFromSig :: TcIdSigInst -> TcM a -> TcM a
tcExtendTyVarEnvFromSig sig_inst thing_inside
  | TISI { sig_inst_skols = skol_prs, sig_inst_wcs = wcs } <- sig_inst
  = tcExtendNameTyVarEnv wcs $
    tcExtendNameTyVarEnv (mapSnd binderVar skol_prs) $
    thing_inside

tcExtendIdBinderStackForRhs :: [MonoBindInfo] -> TcM a -> TcM a
-- See Note [Relevant bindings and the binder stack]
tcExtendIdBinderStackForRhs infos thing_inside
  = tcExtendBinderStack [ TcIdBndr mono_id NotTopLevel
                        | MBI { mbi_mono_id = mono_id } <- infos ]
                        thing_inside
    -- NotTopLevel: it's a monomorphic binding

---------------------
getMonoBindInfo :: [LocatedA TcMonoBind] -> [MonoBindInfo]
getMonoBindInfo tc_binds
  = foldr (get_info . unLoc) [] tc_binds
  where
    get_info (TcFunBind info _ _)    rest = info : rest
    get_info (TcPatBind infos _ _ _) rest = infos ++ rest


{- Note [Relevant bindings and the binder stack]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When typecking a binding we extend the TcBinderStack for the RHS of
the binding, with the /monomorphic/ Id.  That way, if we have, say
    f = \x -> blah
and something goes wrong in 'blah', we get a "relevant binding"
looking like  f :: alpha -> beta
This applies if 'f' has a type signature too:
   f :: forall a. [a] -> [a]
   f x = True
We can't unify True with [a], and a relevant binding is f :: [a] -> [a]
If we had the *polymorphic* version of f in the TcBinderStack, it
would not be reported as relevant, because its type is closed.
(See TcErrors.relevantBindings.)

Note [Typechecking pattern bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Look at:
   - typecheck/should_compile/ExPat
   - #12427, typecheck/should_compile/T12427{a,b}

  data T where
    MkT :: Integral a => a -> Int -> T

and suppose t :: T.  Which of these pattern bindings are ok?

  E1. let { MkT p _ = t } in <body>

  E2. let { MkT _ q = t } in <body>

  E3. let { MkT (toInteger -> r) _ = t } in <body>

* (E1) is clearly wrong because the existential 'a' escapes.
  What type could 'p' possibly have?

* (E2) is fine, despite the existential pattern, because
  q::Int, and nothing escapes.

* Even (E3) is fine.  The existential pattern binds a dictionary
  for (Integral a) which the view pattern can use to convert the
  a-valued field to an Integer, so r :: Integer.

An easy way to see all three is to imagine the desugaring.
For (E2) it would look like
    let q = case t of MkT _ q' -> q'
    in <body>


We typecheck pattern bindings as follows.  First tcLhs does this:

  1. Take each type signature q :: ty, partial or complete, and
     instantiate it (with tcLhsSigId) to get a MonoBindInfo.  This
     gives us a fresh "mono_id" qm :: instantiate(ty), where qm has
     a fresh name.

     Any fresh unification variables in instantiate(ty) born here, not
     deep under implications as would happen if we allocated them when
     we encountered q during tcPat.

  2. Build a little environment mapping "q" -> "qm" for those Ids
     with signatures (inst_sig_fun)

  3. Invoke tcLetPat to typecheck the pattern.

     - We pass in the current TcLevel.  This is captured by
       GHC.Tc.Gen.Pat.tcLetPat, and put into the pc_lvl field of PatCtxt, in
       PatEnv.

     - When tcPat finds an existential constructor, it binds fresh
       type variables and dictionaries as usual, increments the TcLevel,
       and emits an implication constraint.

     - When we come to a binder (GHC.Tc.Gen.Pat.tcPatBndr), it looks it up
       in the little environment (the pc_sig_fn field of PatCtxt).

         Success => There was a type signature, so just use it,
                    checking compatibility with the expected type.

         Failure => No type signature.
             Infer case: (happens only outside any constructor pattern)
                         use a unification variable
                         at the outer level pc_lvl

             Check case: use promoteTcType to promote the type
                         to the outer level pc_lvl.  This is the
                         place where we emit a constraint that'll blow
                         up if existential capture takes place

       Result: the type of the binder is always at pc_lvl. This is
       crucial.

  4. Throughout, when we are making up an Id for the pattern-bound variables
     (newLetBndr), we have two cases:

     - If we are generalising (generalisation plan is InferGen or
       CheckGen), then the let_bndr_spec will be LetLclBndr.  In that case
       we want to bind a cloned, local version of the variable, with the
       type given by the pattern context, *not* by the signature (even if
       there is one; see #7268). The mkExport part of the
       generalisation step will do the checking and impedance matching
       against the signature.

     - If for some reason we are not generalising (plan = NoGen), the
       LetBndrSpec will be LetGblBndr.  In that case we must bind the
       global version of the Id, and do so with precisely the type given
       in the signature.  (Then we unify with the type from the pattern
       context type.)


And that's it!  The implication constraints check for the skolem
escape.  It's quite simple and neat, and more expressive than before
e.g. GHC 8.0 rejects (E2) and (E3).

Example for (E1), starting at level 1.  We generate
     p :: beta:1, with constraints (forall:3 a. Integral a => a ~ beta)
The (a~beta) can't float (because of the 'a'), nor be solved (because
beta is untouchable.)

Example for (E2), we generate
     q :: beta:1, with constraint (forall:3 a. Integral a => Int ~ beta)
The beta is untouchable, but floats out of the constraint and can
be solved absolutely fine.


************************************************************************
*                                                                      *
                Generalisation
*                                                                      *
********************************************************************* -}

data GeneralisationPlan
  = NoGen               -- No generalisation, no AbsBinds

  | InferGen            -- Implicit generalisation; there is an AbsBinds
       Bool             --   True <=> apply the MR; generalise only unconstrained type vars

  | CheckGen (LHsBind GhcRn) TcIdSigInfo
                        -- One FunBind with a signature
                        -- Explicit generalisation

-- A consequence of the no-AbsBinds choice (NoGen) is that there is
-- no "polymorphic Id" and "monmomorphic Id"; there is just the one

instance Outputable GeneralisationPlan where
  ppr NoGen          = text "NoGen"
  ppr (InferGen b)   = text "InferGen" <+> ppr b
  ppr (CheckGen _ s) = text "CheckGen" <+> ppr s

decideGeneralisationPlan
   :: DynFlags -> [LHsBind GhcRn] -> IsGroupClosed -> TcSigFun
   -> GeneralisationPlan
decideGeneralisationPlan dflags lbinds closed sig_fn
  | has_partial_sigs                         = InferGen (and partial_sig_mrs)
  | Just (bind, sig) <- one_funbind_with_sig = CheckGen bind sig
  | do_not_generalise closed                 = NoGen
  | otherwise                                = InferGen mono_restriction
  where
    binds = map unLoc lbinds

    partial_sig_mrs :: [Bool]
    -- One for each partial signature (so empty => no partial sigs)
    -- The Bool is True if the signature has no constraint context
    --      so we should apply the MR
    -- See Note [Partial type signatures and generalisation]
    partial_sig_mrs
      = [ null $ fromMaybeContext mtheta
        | TcIdSig (PartialSig { psig_hs_ty = hs_ty })
            <- mapMaybe sig_fn (collectHsBindListBinders CollNoDictBinders lbinds)
        , let (mtheta, _) = splitLHsQualTy (hsSigWcType hs_ty) ]

    has_partial_sigs   = not (null partial_sig_mrs)

    mono_restriction  = xopt LangExt.MonomorphismRestriction dflags
                     && any restricted binds

    do_not_generalise (IsGroupClosed _ True) = False
        -- The 'True' means that all of the group's
        -- free vars have ClosedTypeId=True; so we can ignore
        -- -XMonoLocalBinds, and generalise anyway
    do_not_generalise _ = xopt LangExt.MonoLocalBinds dflags

    -- With OutsideIn, all nested bindings are monomorphic
    -- except a single function binding with a signature
    one_funbind_with_sig
      | [lbind@(L _ (FunBind { fun_id = v }))] <- lbinds
      , Just (TcIdSig sig) <- sig_fn (unLoc v)
      = Just (lbind, sig)
      | otherwise
      = Nothing

    -- The Haskell 98 monomorphism restriction
    restricted (PatBind {})                              = True
    restricted (VarBind { var_id = v })                  = no_sig v
    restricted (FunBind { fun_id = v, fun_matches = m }) = restricted_match m
                                                           && no_sig (unLoc v)
    restricted b = pprPanic "isRestrictedGroup/unrestricted" (ppr b)

    restricted_match mg = matchGroupArity mg == 0
        -- No args => like a pattern binding
        -- Some args => a function binding

    no_sig n = not (hasCompleteSig sig_fn n)

isClosedBndrGroup :: TcTypeEnv -> Bag (LHsBind GhcRn) -> IsGroupClosed
isClosedBndrGroup type_env binds
  = IsGroupClosed fv_env type_closed
  where
    type_closed = allUFM (nameSetAll is_closed_type_id) fv_env

    fv_env :: NameEnv NameSet
    fv_env = mkNameEnv $ concatMap (bindFvs . unLoc) binds

    bindFvs :: HsBindLR GhcRn GhcRn -> [(Name, NameSet)]
    bindFvs (FunBind { fun_id = L _ f
                     , fun_ext = fvs })
       = let open_fvs = get_open_fvs fvs
         in [(f, open_fvs)]
    bindFvs (PatBind { pat_lhs = pat, pat_ext = fvs })
       = let open_fvs = get_open_fvs fvs
         in [(b, open_fvs) | b <- collectPatBinders CollNoDictBinders pat]
    bindFvs _
       = []

    get_open_fvs fvs = filterNameSet (not . is_closed) fvs

    is_closed :: Name -> ClosedTypeId
    is_closed name
      | Just thing <- lookupNameEnv type_env name
      = case thing of
          AGlobal {}                     -> True
          ATcId { tct_info = ClosedLet } -> True
          _                              -> False

      | otherwise
      = True  -- The free-var set for a top level binding mentions


    is_closed_type_id :: Name -> Bool
    -- We're already removed Global and ClosedLet Ids
    is_closed_type_id name
      | Just thing <- lookupNameEnv type_env name
      = case thing of
          ATcId { tct_info = NonClosedLet _ cl } -> cl
          ATcId { tct_info = NotLetBound }       -> False
          ATyVar {}                              -> False
               -- In-scope type variables are not closed!
          _ -> pprPanic "is_closed_id" (ppr name)

      | otherwise
      = True   -- The free-var set for a top level binding mentions
               -- imported things too, so that we can report unused imports
               -- These won't be in the local type env.
               -- Ditto class method etc from the current module


{- *********************************************************************
*                                                                      *
               Error contexts and messages
*                                                                      *
********************************************************************* -}

-- This one is called on LHS, when pat and grhss are both Name
-- and on RHS, when pat is TcId and grhss is still Name
patMonoBindsCtxt :: (
#if MIN_VERSION_base(4,16,0)
  WFT (XOverLit (GhcPass p)),
  WFT (XOverLit (GhcPass (NoGhcTcPass p))),
  WFT (Anno (IdGhcP p)),
  WFT (Anno (IdGhcP (NoGhcTcPass p))),
#endif
  OutputableBndrId p)
                 => LPat (GhcPass p) -> GRHSs GhcRn (LHsExpr GhcRn) -> SDoc
patMonoBindsCtxt pat grhss
  = hang (text "In a pattern binding:") 2 (pprPatBind pat grhss)
