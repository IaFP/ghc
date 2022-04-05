{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 903
{-# LANGUAGE NoMonomorphismRestriction #-}
#endif
{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-
(c) The University of Glasgow, 1994-2006


Core pass to saturate constructors and PrimOps
-}

module GHC.CoreToStg.Prep
   ( corePrepPgm
   , corePrepExpr
   , mkConvertNumLiteral
   )
where

import GHC.Prelude

import GHC.Platform

import GHC.Driver.Session
import GHC.Driver.Env
import GHC.Driver.Ppr

import GHC.Tc.Utils.Env
import GHC.Unit

import GHC.Builtin.Names
import GHC.Builtin.PrimOps
import GHC.Builtin.Types
import GHC.Builtin.Types.Prim ( realWorldStatePrimTy )

import GHC.Core.Utils
import GHC.Core.Opt.Arity
import GHC.Core.FVs
import GHC.Core.Opt.Monad ( CoreToDo(..) )
import GHC.Core.Lint    ( endPassIO )
import GHC.Core
import GHC.Core.Make hiding( FloatBind(..) )   -- We use our own FloatBind here
import GHC.Core.Type
import GHC.Core.Coercion
import GHC.Core.TyCon
import GHC.Core.DataCon
import GHC.Core.Opt.OccurAnal
import GHC.Core.TyCo.Rep( UnivCoProvenance(..) )

import GHC.Data.Maybe
import GHC.Data.OrdList
import GHC.Data.FastString
import GHC.Data.Pair

import GHC.Utils.Error
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import GHC.Utils.Outputable
import GHC.Utils.Monad  ( mapAccumLM )
import GHC.Utils.Logger
import GHC.Utils.Trace

import GHC.Types.Demand
import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Id.Make ( realWorldPrimId, mkPrimOpId )
import GHC.Types.Basic
import GHC.Types.Name   ( NamedThing(..), nameSrcSpan, isInternalName )
import GHC.Types.SrcLoc ( SrcSpan(..), realSrcLocSpan, mkRealSrcLoc )
import GHC.Types.Literal
import GHC.Types.Tickish
import GHC.Types.TyThing
import GHC.Types.Unique.Supply

import Data.List        ( unfoldr )
import Data.Functor.Identity
import Control.Monad

{-
-- ---------------------------------------------------------------------------
-- Note [CorePrep Overview]
-- ---------------------------------------------------------------------------

The goal of this pass is to prepare for code generation.

1.  Saturate constructor and primop applications.

2.  Convert to A-normal form; that is, function arguments
    are always variables.

    * Use case for strict arguments:
        f E ==> case E of x -> f x
        (where f is strict)

    * Use let for non-trivial lazy arguments
        f E ==> let x = E in f x
        (were f is lazy and x is non-trivial)

3.  Similarly, convert any unboxed lets into cases.
    [I'm experimenting with leaving 'ok-for-speculation'
     rhss in let-form right up to this point.]

4.  Ensure that *value* lambdas only occur as the RHS of a binding
    (The code generator can't deal with anything else.)
    Type lambdas are ok, however, because the code gen discards them.

5.  [Not any more; nuked Jun 2002] Do the seq/par munging.

6.  Clone all local Ids.
    This means that all such Ids are unique, rather than the
    weaker guarantee of no clashes which the simplifier provides.
    And that is what the code generator needs.

    We don't clone TyVars or CoVars. The code gen doesn't need that,
    and doing so would be tiresome because then we'd need
    to substitute in types and coercions.

7.  Give each dynamic CCall occurrence a fresh unique; this is
    rather like the cloning step above.

8.  Inject bindings for the "implicit" Ids:
        * Constructor wrappers
        * Constructor workers
    We want curried definitions for all of these in case they
    aren't inlined by some caller.

9.  Replace (lazy e) by e.  See Note [lazyId magic] in GHC.Types.Id.Make
    Also replace (noinline e) by e.

10. Convert bignum literals into their core representation.

11. Uphold tick consistency while doing this: We move ticks out of
    (non-type) applications where we can, and make sure that we
    annotate according to scoping rules when floating.

12. Collect cost centres (including cost centres in unfoldings) if we're in
    profiling mode. We have to do this here beucase we won't have unfoldings
    after this pass (see `zapUnfolding` and Note [Drop unfoldings and rules].

13. Eliminate case clutter in favour of unsafe coercions.
    See Note [Unsafe coercions]

14. Eliminate some magic Ids, specifically
     runRW# (\s. e)  ==>  e[readWorldId/s]
             lazy e  ==>  e
         noinline e  ==>  e
     ToDo:  keepAlive# ...
    This is done in cpeApp

This is all done modulo type applications and abstractions, so that
when type erasure is done for conversion to STG, we don't end up with
any trivial or useless bindings.

Note [Unsafe coercions]
~~~~~~~~~~~~~~~~~~~~~~~
CorePrep does these two transformations:

1. Convert empty case to cast with an unsafe coercion
          (case e of {}) ===>  e |> unsafe-co
   See Note [Empty case alternatives] in GHC.Core: if the case
   alternatives are empty, the scrutinee must diverge or raise an
   exception, so we can just dive into it.

   Of course, if the scrutinee *does* return, we may get a seg-fault.
   A belt-and-braces approach would be to persist empty-alternative
   cases to code generator, and put a return point anyway that calls a
   runtime system error function.

   Notice that eliminating empty case can lead to an ill-kinded coercion
       case error @Int "foo" of {}  :: Int#
       ===> error @Int "foo" |> unsafe-co
       where unsafe-co :: Int ~ Int#
   But that's fine because the expression diverges anyway. And it's
   no different to what happened before.

2. Eliminate unsafeEqualityProof in favour of an unsafe coercion
           case unsafeEqualityProof of UnsafeRefl g -> e
           ===>  e[unsafe-co/g]
   See (U2) in Note [Implementing unsafeCoerce] in base:Unsafe.Coerce

   Note that this requires us to substitute 'unsafe-co' for 'g', and
   that is the main (current) reason for cpe_tyco_env in CorePrepEnv.
   Tiresome, but not difficult.

These transformations get rid of "case clutter", leaving only casts.
We are doing no further significant tranformations, so the reasons
for the case forms have disappeared. And it is extremely helpful for
the ANF-ery, CoreToStg, and backends, if trivial expressions really do
look trivial. #19700 was an example.

In both cases, the "unsafe-co" is just (UnivCo ty1 ty2 (CorePrepProv b)),
The boolean 'b' says whether the unsafe coercion is supposed to be
kind-homogeneous (yes for (2), no for (1).  This information is used
/only/ by Lint.

Note [CorePrep invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Here is the syntax of the Core produced by CorePrep:

    Trivial expressions
       arg ::= lit |  var
              | arg ty  |  /\a. arg
              | truv co  |  /\c. arg  |  arg |> co

    Applications
       app ::= lit  |  var  |  app arg  |  app ty  | app co | app |> co

    Expressions
       body ::= app
              | let(rec) x = rhs in body     -- Boxed only
              | case body of pat -> body
              | /\a. body | /\c. body
              | body |> co

    Right hand sides (only place where value lambdas can occur)
       rhs ::= /\a.rhs  |  \x.rhs  |  body

We define a synonym for each of these non-terminals.  Functions
with the corresponding name produce a result in that syntax.
-}

type CpeArg  = CoreExpr    -- Non-terminal 'arg'
type CpeApp  = CoreExpr    -- Non-terminal 'app'
type CpeBody = CoreExpr    -- Non-terminal 'body'
type CpeRhs  = CoreExpr    -- Non-terminal 'rhs'

{-
************************************************************************
*                                                                      *
                Top level stuff
*                                                                      *
************************************************************************
-}

corePrepPgm :: HscEnv -> Module -> ModLocation -> CoreProgram -> [TyCon]
            -> IO CoreProgram
corePrepPgm hsc_env this_mod mod_loc binds data_tycons =
    withTiming logger
               (text "CorePrep"<+>brackets (ppr this_mod))
               (\a -> a `seqList` ()) $ do
    us <- mkSplitUniqSupply 's'
    initialCorePrepEnv <- mkInitialCorePrepEnv hsc_env

    let
        implicit_binds = mkDataConWorkers dflags mod_loc data_tycons
            -- NB: we must feed mkImplicitBinds through corePrep too
            -- so that they are suitably cloned and eta-expanded

        binds_out = initUs_ us $ do
                      floats1 <- corePrepTopBinds initialCorePrepEnv binds
                      floats2 <- corePrepTopBinds initialCorePrepEnv implicit_binds
                      return (deFloatTop (floats1 `appendFloats` floats2))

    endPassIO hsc_env alwaysQualify CorePrep binds_out []
    return binds_out
  where
    dflags = hsc_dflags hsc_env
    logger = hsc_logger hsc_env

corePrepExpr :: HscEnv -> CoreExpr -> IO CoreExpr
corePrepExpr hsc_env expr = do
    let logger = hsc_logger hsc_env
    withTiming logger (text "CorePrep [expr]") (\e -> e `seq` ()) $ do
      us <- mkSplitUniqSupply 's'
      initialCorePrepEnv <- mkInitialCorePrepEnv hsc_env
      let new_expr = initUs_ us (cpeBodyNF initialCorePrepEnv expr)
      putDumpFileMaybe logger Opt_D_dump_prep "CorePrep" FormatCore (ppr new_expr)
      return new_expr

corePrepTopBinds :: CorePrepEnv -> [CoreBind] -> UniqSM Floats
-- Note [Floating out of top level bindings]
corePrepTopBinds initialCorePrepEnv binds
  = go initialCorePrepEnv binds
  where
    go _   []             = return emptyFloats
    go env (bind : binds) = do (env', floats, maybe_new_bind)
                                 <- cpeBind TopLevel env bind
                               massert (isNothing maybe_new_bind)
                                 -- Only join points get returned this way by
                                 -- cpeBind, and no join point may float to top
                               floatss <- go env' binds
                               return (floats `appendFloats` floatss)

mkDataConWorkers :: DynFlags -> ModLocation -> [TyCon] -> [CoreBind]
-- See Note [Data constructor workers]
-- c.f. Note [Injecting implicit bindings] in GHC.Iface.Tidy
mkDataConWorkers dflags mod_loc data_tycons
  = [ NonRec id (tick_it (getName data_con) (Var id))
                                -- The ice is thin here, but it works
    | tycon <- data_tycons,     -- CorePrep will eta-expand it
      data_con <- tyConDataCons tycon,
      let id = dataConWorkId data_con
    ]
 where
   -- If we want to generate debug info, we put a source note on the
   -- worker. This is useful, especially for heap profiling.
   tick_it name
     | debugLevel dflags == 0                = id
     | RealSrcSpan span _ <- nameSrcSpan name = tick span
     | Just file <- ml_hs_file mod_loc       = tick (span1 file)
     | otherwise                             = tick (span1 "???")
     where tick span  = Tick (SourceNote span $ showSDoc dflags (ppr name))
           span1 file = realSrcLocSpan $ mkRealSrcLoc (mkFastString file) 1 1

{-
Note [Floating out of top level bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NB: we do need to float out of top-level bindings
Consider        x = length [True,False]
We want to get
                s1 = False : []
                s2 = True  : s1
                x  = length s2

We return a *list* of bindings, because we may start with
        x* = f (g y)
where x is demanded, in which case we want to finish with
        a = g y
        x* = f a
And then x will actually end up case-bound

Note [Join points and floating]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Join points can float out of other join points but not out of value bindings:

  let z =
    let  w = ... in -- can float
    join k = ... in -- can't float
    ... jump k ...
  join j x1 ... xn =
    let  y = ... in -- can float (but don't want to)
    join h = ... in -- can float (but not much point)
    ... jump h ...
  in ...

Here, the jump to h remains valid if h is floated outward, but the jump to k
does not.

We don't float *out* of join points. It would only be safe to float out of
nullary join points (or ones where the arguments are all either type arguments
or dead binders). Nullary join points aren't ever recursive, so they're always
effectively one-shot functions, which we don't float out of. We *could* float
join points from nullary join points, but there's no clear benefit at this
stage.

Note [Data constructor workers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Create any necessary "implicit" bindings for data con workers.  We
create the rather strange (non-recursive!) binding

        $wC = \x y -> $wC x y

i.e. a curried constructor that allocates.  This means that we can
treat the worker for a constructor like any other function in the rest
of the compiler.  The point here is that CoreToStg will generate a
StgConApp for the RHS, rather than a call to the worker (which would
give a loop).  As Lennart says: the ice is thin here, but it works.

Hmm.  Should we create bindings for dictionary constructors?  They are
always fully applied, and the bindings are just there to support
partial applications. But it's easier to let them through.


Note [Dead code in CorePrep]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Imagine that we got an input program like this (see #4962):

  f :: Show b => Int -> (Int, b -> Maybe Int -> Int)
  f x = (g True (Just x) + g () (Just x), g)
    where
      g :: Show a => a -> Maybe Int -> Int
      g _ Nothing = x
      g y (Just z) = if z > 100 then g y (Just (z + length (show y))) else g y unknown

After specialisation and SpecConstr, we would get something like this:

  f :: Show b => Int -> (Int, b -> Maybe Int -> Int)
  f x = (g$Bool_True_Just x + g$Unit_Unit_Just x, g)
    where
      {-# RULES g $dBool = g$Bool
                g $dUnit = g$Unit #-}
      g = ...
      {-# RULES forall x. g$Bool True (Just x) = g$Bool_True_Just x #-}
      g$Bool = ...
      {-# RULES forall x. g$Unit () (Just x) = g$Unit_Unit_Just x #-}
      g$Unit = ...
      g$Bool_True_Just = ...
      g$Unit_Unit_Just = ...

Note that the g$Bool and g$Unit functions are actually dead code: they
are only kept alive by the occurrence analyser because they are
referred to by the rules of g, which is being kept alive by the fact
that it is used (unspecialised) in the returned pair.

However, at the CorePrep stage there is no way that the rules for g
will ever fire, and it really seems like a shame to produce an output
program that goes to the trouble of allocating a closure for the
unreachable g$Bool and g$Unit functions.

The way we fix this is to:
 * In cloneBndr, drop all unfoldings/rules

 * In deFloatTop, run a simple dead code analyser on each top-level
   RHS to drop the dead local bindings.

The reason we don't just OccAnal the whole output of CorePrep is that
the tidier ensures that all top-level binders are GlobalIds, so they
don't show up in the free variables any longer. So if you run the
occurrence analyser on the output of CoreTidy (or later) you e.g. turn
this program:

  Rec {
  f = ... f ...
  }

Into this one:

  f = ... f ...

(Since f is not considered to be free in its own RHS.)


Note [keepAlive# magic]
~~~~~~~~~~~~~~~~~~~~~~~
When interacting with foreign code, it is often necessary for the user to
extend the lifetime of a heap object beyond the lifetime that would be apparent
from the on-heap references alone. For instance, a program like:

  foreign import safe "hello" hello :: ByteArray# -> IO ()

  callForeign :: IO ()
  callForeign = IO $ \s0 ->
    case newByteArray# n# s0 of (# s1, barr #) ->
      unIO hello barr s1

As-written this program is susceptible to memory-unsafety since there are
no references to `barr` visible to the garbage collector. Consequently, if a
garbage collection happens during the execution of the C function `hello`, it
may be that the array is freed while in use by the foreign function.

To address this, we introduced a new primop, keepAlive#, which "scopes over"
the computation needing the kept-alive value:

  keepAlive# :: forall (ra :: RuntimeRep) (rb :: RuntimeRep) (a :: TYPE a) (b :: TYPE b).
                a -> State# RealWorld -> (State# RealWorld -> b) -> b

When entered, an application (keepAlive# x s k) will apply `k` to the state
token, evaluating it to WHNF. However, during the course of this evaluation
will *guarantee* that `x` is considered to be alive.

There are a few things to note here:

 - we are RuntimeRep-polymorphic in the value to be kept-alive. This is
   necessary since we will often (but not always) be keeping alive something
   unlifted (like a ByteArray#)

 - we are RuntimeRep-polymorphic in the result value since the result may take
   many forms (e.g. a boxed value, a raw state token, or a (# State s, result #).

We implement this operation by desugaring to touch# during CorePrep (see
GHC.CoreToStg.Prep.cpeApp). Specifically,

  keepAlive# x s0 k

is transformed to:

  case k s0 of r ->
  case touch# x realWorld# of s1 ->
    r

Operationally, `keepAlive# x s k` is equivalent to pushing a stack frame with a
pointer to `x` and entering `k s0`. This compilation strategy is safe
because we do no optimization on STG that would drop or re-order the
continuation containing the `touch#`. However, if we were to become more
aggressive in our STG pipeline then we would need to revisit this.

Beyond this CorePrep transformation, there is very little special about
keepAlive#. However, we did explore (and eventually gave up on)
an optimisation which would allow unboxing of constructed product results,
which we describe below.


Lost optimisation: CPR unboxing
--------------------------------
One unfortunate property of this approach is that the simplifier is unable to
unbox the result of a keepAlive# expression. For instance, consider the program:

  case keepAlive# arr s0 (
         \s1 -> case peekInt arr s1 of
                  (# s2, r #) -> I# r
  ) of
    I# x -> ...

This is a surprisingly common pattern, previously used, e.g., in
GHC.IO.Buffer.readWord8Buf. While exploring ideas, we briefly played around
with optimising this away by pushing strict contexts (like the
`case [] of I# x -> ...` above) into keepAlive#'s continuation. While this can
recover unboxing, it can also unfortunately in general change the asymptotic
memory (namely stack) behavior of the program. For instance, consider

  writeN =
    ...
      case keepAlive# x s0 (\s1 -> something s1) of
        (# s2, x #) ->
          writeN ...

As it is tail-recursive, this program will run in constant space. However, if
we push outer case into the continuation we get:

  writeN =

      case keepAlive# x s0 (\s1 ->
        case something s1 of
          (# s2, x #) ->
            writeN ...
      ) of
        ...

Which ends up building a stack which is linear in the recursion depth. For this
reason, we ended up giving up on this optimisation.


Historical note: touch# and its inadequacy
------------------------------------------
Prior to the introduction of `keepAlive#` we instead addressed the need for
lifetime extension with the `touch#` primop:

    touch# :: a -> State# s -> State# s

This operation would ensure that the `a` value passed as the first argument was
considered "alive" at the time the primop application is entered.

For instance, the user might modify `callForeign` as:

  callForeign :: IO ()
  callForeign s0 = IO $ \s0 ->
    case newByteArray# n# s0 of (# s1, barr #) ->
    case unIO hello barr s1 of (# s2, () #) ->
    case touch# barr s2 of s3 ->
      (# s3, () #)

However, in #14346 we discovered that this primop is insufficient in the
presence of simplification. For instance, consider a program like:

  callForeign :: IO ()
  callForeign s0 = IO $ \s0 ->
    case newByteArray# n# s0 of (# s1, barr #) ->
    case unIO (forever $ hello barr) s1 of (# s2, () #) ->
    case touch# barr s2 of s3 ->
      (# s3, () #)

In this case the Simplifier may realize that (forever $ hello barr)
will never return and consequently that the `touch#` that follows is dead code.
As such, it will be dropped, resulting in memory unsoundness.
This unsoundness lead to the introduction of keepAlive#.



Other related tickets:

 - #15544
 - #17760
 - #14375
 - #15260
 - #18061

************************************************************************
*                                                                      *
                The main code
*                                                                      *
************************************************************************
-}

cpeBind :: TopLevelFlag -> CorePrepEnv -> CoreBind
        -> UniqSM (CorePrepEnv,
                   Floats,         -- Floating value bindings
                   Maybe CoreBind) -- Just bind' <=> returned new bind; no float
                                   -- Nothing <=> added bind' to floats instead
cpeBind top_lvl env (NonRec bndr rhs)
  | not (isJoinId bndr)
  = do { (env1, bndr1) <- cpCloneBndr env bndr
       ; let dmd         = idDemandInfo bndr
             is_unlifted = isUnliftedType (idType bndr)
       ; (floats, rhs1) <- cpePair top_lvl NonRecursive
                                   dmd is_unlifted
                                   env bndr1 rhs
       -- See Note [Inlining in CorePrep]
       ; let triv_rhs = exprIsTrivial rhs1
             env2    | triv_rhs  = extendCorePrepEnvExpr env1 bndr rhs1
                     | otherwise = env1
             floats1 | triv_rhs, isInternalName (idName bndr)
                     = floats
                     | otherwise
                     = addFloat floats new_float

             new_float = mkFloat dmd is_unlifted bndr1 rhs1

       ; return (env2, floats1, Nothing) }

  | otherwise -- A join point; see Note [Join points and floating]
  = assert (not (isTopLevel top_lvl)) $ -- can't have top-level join point
    do { (_, bndr1) <- cpCloneBndr env bndr
       ; (bndr2, rhs1) <- cpeJoinPair env bndr1 rhs
       ; return (extendCorePrepEnv env bndr bndr2,
                 emptyFloats,
                 Just (NonRec bndr2 rhs1)) }

cpeBind top_lvl env (Rec pairs)
  | not (isJoinId (head bndrs))
  = do { (env', bndrs1) <- cpCloneBndrs env bndrs
       ; stuff <- zipWithM (cpePair top_lvl Recursive topDmd False env')
                           bndrs1 rhss

       ; let (floats_s, rhss1) = unzip stuff
             all_pairs = foldrOL add_float (bndrs1 `zip` rhss1)
                                           (concatFloats floats_s)

       ; return (extendCorePrepEnvList env (bndrs `zip` bndrs1),
                 unitFloat (FloatLet (Rec all_pairs)),
                 Nothing) }

  | otherwise -- See Note [Join points and floating]
  = do { (env', bndrs1) <- cpCloneBndrs env bndrs
       ; pairs1 <- zipWithM (cpeJoinPair env') bndrs1 rhss

       ; let bndrs2 = map fst pairs1
       ; return (extendCorePrepEnvList env' (bndrs `zip` bndrs2),
                 emptyFloats,
                 Just (Rec pairs1)) }
  where
    (bndrs, rhss) = unzip pairs

        -- Flatten all the floats, and the current
        -- group into a single giant Rec
    add_float (FloatLet (NonRec b r)) prs2 = (b,r) : prs2
    add_float (FloatLet (Rec prs1))   prs2 = prs1 ++ prs2
    add_float b                       _    = pprPanic "cpeBind" (ppr b)

---------------
cpePair :: TopLevelFlag -> RecFlag -> Demand -> Bool
        -> CorePrepEnv -> OutId -> CoreExpr
        -> UniqSM (Floats, CpeRhs)
-- Used for all bindings
-- The binder is already cloned, hence an OutId
cpePair top_lvl is_rec dmd is_unlifted env bndr rhs
  = assert (not (isJoinId bndr)) $ -- those should use cpeJoinPair
    do { (floats1, rhs1) <- cpeRhsE env rhs

       -- See if we are allowed to float this stuff out of the RHS
       ; (floats2, rhs2) <- float_from_rhs floats1 rhs1

       -- Make the arity match up
       ; (floats3, rhs3)
            <- if manifestArity rhs1 <= arity
               then return (floats2, cpeEtaExpand arity rhs2)
               else warnPprTrace True (text "CorePrep: silly extra arguments:" <+> ppr bndr) $
                               -- Note [Silly extra arguments]
                    (do { v <- newVar (idType bndr)
                        ; let float = mkFloat topDmd False v rhs2
                        ; return ( addFloat floats2 float
                                 , cpeEtaExpand arity (Var v)) })

        -- Wrap floating ticks
       ; let (floats4, rhs4) = wrapTicks floats3 rhs3

       ; return (floats4, rhs4) }
  where
    arity = idArity bndr        -- We must match this arity

    ---------------------
    float_from_rhs floats rhs
      | isEmptyFloats floats = return (emptyFloats, rhs)
      | isTopLevel top_lvl   = float_top    floats rhs
      | otherwise            = float_nested floats rhs

    ---------------------
    float_nested floats rhs
      | wantFloatNested is_rec dmd is_unlifted floats rhs
                  = return (floats, rhs)
      | otherwise = dontFloat floats rhs

    ---------------------
    float_top floats rhs
      | allLazyTop floats
      = return (floats, rhs)

      | Just floats <- canFloat floats rhs
      = return floats

      | otherwise
      = dontFloat floats rhs

dontFloat :: Floats -> CpeRhs -> UniqSM (Floats, CpeBody)
-- Non-empty floats, but do not want to float from rhs
-- So wrap the rhs in the floats
-- But: rhs1 might have lambdas, and we can't
--      put them inside a wrapBinds
dontFloat floats1 rhs
  = do { (floats2, body) <- rhsToBody rhs
        ; return (emptyFloats, wrapBinds floats1 $
                               wrapBinds floats2 body) }

{- Note [Silly extra arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we had this
        f{arity=1} = \x\y. e
We *must* match the arity on the Id, so we have to generate
        f' = \x\y. e
        f  = \x. f' x

It's a bizarre case: why is the arity on the Id wrong?  Reason
(in the days of __inline_me__):
        f{arity=0} = __inline_me__ (let v = expensive in \xy. e)
When InlineMe notes go away this won't happen any more.  But
it seems good for CorePrep to be robust.
-}

---------------
cpeJoinPair :: CorePrepEnv -> JoinId -> CoreExpr
            -> UniqSM (JoinId, CpeRhs)
-- Used for all join bindings
-- No eta-expansion: see Note [Do not eta-expand join points] in GHC.Core.Opt.Simplify.Utils
cpeJoinPair env bndr rhs
  = assert (isJoinId bndr) $
    do { let Just join_arity = isJoinId_maybe bndr
             (bndrs, body)   = collectNBinders join_arity rhs

       ; (env', bndrs') <- cpCloneBndrs env bndrs

       ; body' <- cpeBodyNF env' body -- Will let-bind the body if it starts
                                      -- with a lambda

       ; let rhs'  = mkCoreLams bndrs' body'
             bndr' = bndr `setIdUnfolding` evaldUnfolding
                          `setIdArity` count isId bndrs
                            -- See Note [Arity and join points]

       ; return (bndr', rhs') }

{-
Note [Arity and join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Up to now, we've allowed a join point to have an arity greater than its join
arity (minus type arguments), since this is what's useful for eta expansion.
However, for code gen purposes, its arity must be exactly the number of value
arguments it will be called with, and it must have exactly that many value
lambdas. Hence if there are extra lambdas we must let-bind the body of the RHS:

  join j x y z = \w -> ... in ...
    =>
  join j x y z = (let f = \w -> ... in f) in ...

This is also what happens with Note [Silly extra arguments]. Note that it's okay
for us to mess with the arity because a join point is never exported.
-}

-- ---------------------------------------------------------------------------
--              CpeRhs: produces a result satisfying CpeRhs
-- ---------------------------------------------------------------------------

cpeRhsE :: CorePrepEnv -> CoreExpr -> UniqSM (Floats, CpeRhs)
-- If
--      e  ===>  (bs, e')
-- then
--      e = let bs in e'        (semantically, that is!)
--
-- For example
--      f (g x)   ===>   ([v = g x], f v)

cpeRhsE env (Type ty)
  = return (emptyFloats, Type (cpSubstTy env ty))
cpeRhsE env (Coercion co)
  = return (emptyFloats, Coercion (cpSubstCo env co))
cpeRhsE env expr@(Lit (LitNumber nt i))
   = case cpe_convertNumLit env nt i of
      Nothing -> return (emptyFloats, expr)
      Just e  -> cpeRhsE env e
cpeRhsE _env expr@(Lit {}) = return (emptyFloats, expr)
cpeRhsE env expr@(Var {})  = cpeApp env expr
cpeRhsE env expr@(App {}) = cpeApp env expr

cpeRhsE env (Let bind body)
  = do { (env', bind_floats, maybe_bind') <- cpeBind NotTopLevel env bind
       ; (body_floats, body') <- cpeRhsE env' body
       ; let expr' = case maybe_bind' of Just bind' -> Let bind' body'
                                         Nothing    -> body'
       ; return (bind_floats `appendFloats` body_floats, expr') }

cpeRhsE env (Tick tickish expr)
  | tickishPlace tickish == PlaceNonLam && tickish `tickishScopesLike` SoftScope
  = do { (floats, body) <- cpeRhsE env expr
         -- See [Floating Ticks in CorePrep]
       ; return (unitFloat (FloatTick tickish) `appendFloats` floats, body) }
  | otherwise
  = do { body <- cpeBodyNF env expr
       ; return (emptyFloats, mkTick tickish' body) }
  where
    tickish' | Breakpoint ext n fvs <- tickish
             -- See also 'substTickish'
             = Breakpoint ext n (map (getIdFromTrivialExpr . lookupCorePrepEnv env) fvs)
             | otherwise
             = tickish

cpeRhsE env (Cast expr co)
   = do { (floats, expr') <- cpeRhsE env expr
        ; return (floats, Cast expr' (cpSubstCo env co)) }

cpeRhsE env expr@(Lam {})
   = do { let (bndrs,body) = collectBinders expr
        ; (env', bndrs') <- cpCloneBndrs env bndrs
        ; body' <- cpeBodyNF env' body
        ; return (emptyFloats, mkLams bndrs' body') }

-- Eliminate empty case
-- See Note [Unsafe coercions]
cpeRhsE env (Case scrut _ ty [])
  = do { (floats, scrut') <- cpeRhsE env scrut
       ; let ty'       = cpSubstTy env ty
             scrut_ty' = exprType scrut'
             co'       = mkUnivCo prov Representational scrut_ty' ty'
             prov      = CorePrepProv False
               -- False says that the kinds of two types may differ
               -- E.g. we might cast Int to Int#.  This is fine
               -- because the scrutinee is guaranteed to diverge

       ; return (floats, Cast scrut' co') }
   -- This can give rise to
   --   Warning: Unsafe coercion: between unboxed and boxed value
   -- but it's fine because 'scrut' diverges

-- Eliminate unsafeEqualityProof
-- See Note [Unsafe coercions]
cpeRhsE env (Case scrut bndr _ alts)
  | isUnsafeEqualityProof scrut
  , isDeadBinder bndr -- We can only discard the case if the case-binder
                      -- is dead.  It usually is, but see #18227
  , [Alt _ [co_var] rhs] <- alts
  , let Pair ty1 ty2 = coVarTypes co_var
        the_co = mkUnivCo prov Nominal (cpSubstTy env ty1) (cpSubstTy env ty2)
        prov   = CorePrepProv True  -- True <=> kind homogeneous
        env'   = extendCoVarEnv env co_var the_co
  = cpeRhsE env' rhs

cpeRhsE env (Case scrut bndr ty alts)
  = do { (floats, scrut') <- cpeBody env scrut
       ; (env', bndr2) <- cpCloneBndr env bndr
       ; let alts'
                 -- This flag is intended to aid in debugging strictness
                 -- analysis bugs. These are particularly nasty to chase down as
                 -- they may manifest as segmentation faults. When this flag is
                 -- enabled we instead produce an 'error' expression to catch
                 -- the case where a function we think should bottom
                 -- unexpectedly returns.
               | gopt Opt_CatchBottoms (cpe_dynFlags env)
               , not (altsAreExhaustive alts)
               = addDefault alts (Just err)
               | otherwise = alts
               where err = mkRuntimeErrorApp rUNTIME_ERROR_ID ty
                                             "Bottoming expression returned"
       ; alts'' <- mapM (sat_alt env') alts'

       ; return (floats, Case scrut' bndr2 ty alts'') }
  where
    sat_alt env (Alt con bs rhs)
       = do { (env2, bs') <- cpCloneBndrs env bs
            ; rhs' <- cpeBodyNF env2 rhs
            ; return (Alt con bs' rhs') }

-- ---------------------------------------------------------------------------
--              CpeBody: produces a result satisfying CpeBody
-- ---------------------------------------------------------------------------

-- | Convert a 'CoreExpr' so it satisfies 'CpeBody', without
-- producing any floats (any generated floats are immediately
-- let-bound using 'wrapBinds').  Generally you want this, esp.
-- when you've reached a binding form (e.g., a lambda) and
-- floating any further would be incorrect.
cpeBodyNF :: CorePrepEnv -> CoreExpr -> UniqSM CpeBody
cpeBodyNF env expr
  = do { (floats, body) <- cpeBody env expr
       ; return (wrapBinds floats body) }

-- | Convert a 'CoreExpr' so it satisfies 'CpeBody'; also produce
-- a list of 'Floats' which are being propagated upwards.  In
-- fact, this function is used in only two cases: to
-- implement 'cpeBodyNF' (which is what you usually want),
-- and in the case when a let-binding is in a case scrutinee--here,
-- we can always float out:
--
--      case (let x = y in z) of ...
--      ==> let x = y in case z of ...
--
cpeBody :: CorePrepEnv -> CoreExpr -> UniqSM (Floats, CpeBody)
cpeBody env expr
  = do { (floats1, rhs) <- cpeRhsE env expr
       ; (floats2, body) <- rhsToBody rhs
       ; return (floats1 `appendFloats` floats2, body) }

--------
rhsToBody :: CpeRhs -> UniqSM (Floats, CpeBody)
-- Remove top level lambdas by let-binding

rhsToBody (Tick t expr)
  | tickishScoped t == NoScope  -- only float out of non-scoped annotations
  = do { (floats, expr') <- rhsToBody expr
       ; return (floats, mkTick t expr') }

rhsToBody (Cast e co)
        -- You can get things like
        --      case e of { p -> coerce t (\s -> ...) }
  = do { (floats, e') <- rhsToBody e
       ; return (floats, Cast e' co) }

rhsToBody expr@(Lam {})
  | Just no_lam_result <- tryEtaReducePrep bndrs body
  = return (emptyFloats, no_lam_result)
  | all isTyVar bndrs           -- Type lambdas are ok
  = return (emptyFloats, expr)
  | otherwise                   -- Some value lambdas
  = do { let rhs = cpeEtaExpand (exprArity expr) expr
       ; fn <- newVar (exprType rhs)
       ; let float = FloatLet (NonRec fn rhs)
       ; return (unitFloat float, Var fn) }
  where
    (bndrs,body) = collectBinders expr

rhsToBody expr = return (emptyFloats, expr)



-- ---------------------------------------------------------------------------
--              CpeApp: produces a result satisfying CpeApp
-- ---------------------------------------------------------------------------

data ArgInfo = CpeApp  CoreArg
             | CpeCast Coercion
             | CpeTick CoreTickish

instance Outputable ArgInfo where
  ppr (CpeApp arg) = text "app" <+> ppr arg
  ppr (CpeCast co) = text "cast" <+> ppr co
  ppr (CpeTick tick) = text "tick" <+> ppr tick

cpeApp :: CorePrepEnv -> CoreExpr -> UniqSM (Floats, CpeRhs)
-- May return a CpeRhs because of saturating primops
cpeApp top_env expr
  = do { let (terminal, args, depth) = collect_args expr
       ; cpe_app top_env terminal args depth
       }

  where
    -- We have a nested data structure of the form
    -- e `App` a1 `App` a2 ... `App` an, convert it into
    -- (e, [CpeApp a1, CpeApp a2, ..., CpeApp an], depth)
    -- We use 'ArgInfo' because we may also need to
    -- record casts and ticks.  Depth counts the number
    -- of arguments that would consume strictness information
    -- (so, no type or coercion arguments.)
    collect_args :: CoreExpr -> (CoreExpr, [ArgInfo], Int)
    collect_args e = go e [] 0
      where
        go (App fun arg)      as !depth
            = go fun (CpeApp arg : as)
                (if isTyCoArg arg then depth else depth + 1)
        go (Cast fun co)      as depth
            = go fun (CpeCast co : as) depth
        go (Tick tickish fun) as depth
            | tickishPlace tickish == PlaceNonLam
            && tickish `tickishScopesLike` SoftScope
            = go fun (CpeTick tickish : as) depth
        go terminal as depth = (terminal, as, depth)

    cpe_app :: CorePrepEnv
            -> CoreExpr
            -> [ArgInfo]
            -> Int
            -> UniqSM (Floats, CpeRhs)
    cpe_app env (Var f) (CpeApp Type{} : CpeApp arg : args) depth
        | f `hasKey` lazyIdKey          -- Replace (lazy a) with a, and
            -- See Note [lazyId magic] in GHC.Types.Id.Make
       || f `hasKey` noinlineIdKey      -- Replace (noinline a) with a
            -- See Note [noinlineId magic] in GHC.Types.Id.Make

        -- Consider the code:
        --
        --      lazy (f x) y
        --
        -- We need to make sure that we need to recursively collect arguments on
        -- "f x", otherwise we'll float "f x" out (it's not a variable) and
        -- end up with this awful -ddump-prep:
        --
        --      case f x of f_x {
        --        __DEFAULT -> f_x y
        --      }
        --
        -- rather than the far superior "f x y".  Test case is par01.
        = let (terminal, args', depth') = collect_args arg
          in cpe_app env terminal (args' ++ args) (depth + depth' - 1)

    -- See Note [keepAlive# magic].
    cpe_app env
            (Var f)
            args
            n
        | Just KeepAliveOp <- isPrimOpId_maybe f
        , CpeApp (Type arg_rep)
          : CpeApp (Type arg_ty)
          : CpeApp (Type _result_rep)
          : CpeApp (Type result_ty)
          : CpeApp arg
          : CpeApp s0
          : CpeApp k
          : rest <- args
        = do { y  <- newVar (cpSubstTy env result_ty)
             ; s2 <- newVar realWorldStatePrimTy
             ; -- beta reduce if possible
             ; (floats, k') <- case k of
                  Lam s body -> cpe_app (extendCorePrepEnvExpr env s s0) body rest (n-2)
                  _          -> cpe_app env k (CpeApp s0 : rest) (n-1)
             ; let touchId = mkPrimOpId TouchOp
                   expr = Case k' y result_ty [Alt DEFAULT [] rhs]
                   rhs = let scrut = mkApps (Var touchId) [Type arg_rep, Type arg_ty, arg, Var realWorldPrimId]
                         in Case scrut s2 result_ty [Alt DEFAULT [] (Var y)]
             ; (floats', expr') <- cpeBody env expr
             ; return (floats `appendFloats` floats', expr')
             }
        | Just KeepAliveOp <- isPrimOpId_maybe f
        = panic "invalid keepAlive# application"

    cpe_app env (Var f) (CpeApp _runtimeRep@Type{} : CpeApp _type@Type{} : CpeApp arg : rest) n
        | f `hasKey` runRWKey
        -- N.B. While it may appear that n == 1 in the case of runRW#
        -- applications, keep in mind that we may have applications that return
        , n >= 1
        -- See Note [runRW magic]
        -- Replace (runRW# f) by (f realWorld#), beta reducing if possible (this
        -- is why we return a CorePrepEnv as well)
        = case arg of
            Lam s body -> cpe_app (extendCorePrepEnv env s realWorldPrimId) body rest (n-2)
            _          -> cpe_app env arg (CpeApp (Var realWorldPrimId) : rest) (n-1)
             -- TODO: What about casts?

    cpe_app env (Var v) args depth
      = do { v1 <- fiddleCCall v
           ; let e2 = lookupCorePrepEnv env v1
                 hd = getIdFromTrivialExpr_maybe e2
           -- NB: depth from collect_args is right, because e2 is a trivial expression
           -- and thus its embedded Id *must* be at the same depth as any
           -- Apps it is under are type applications only (c.f.
           -- exprIsTrivial).  But note that we need the type of the
           -- expression, not the id.
           ; (app, floats) <- rebuild_app env args e2 emptyFloats stricts
           ; mb_saturate hd app floats depth }
        where
          stricts = case idDmdSig v of
                            DmdSig (DmdType _ demands _)
                              | listLengthCmp demands depth /= GT -> demands
                                    -- length demands <= depth
                              | otherwise                         -> []
                -- If depth < length demands, then we have too few args to
                -- satisfy strictness  info so we have to  ignore all the
                -- strictness info, e.g. + (error "urk")
                -- Here, we can't evaluate the arg strictly, because this
                -- partial application might be seq'd

        -- We inlined into something that's not a var and has no args.
        -- Bounce it back up to cpeRhsE.
    cpe_app env fun [] _ = cpeRhsE env fun

        -- N-variable fun, better let-bind it
    cpe_app env fun args depth
      = do { (fun_floats, fun') <- cpeArg env evalDmd fun
                          -- The evalDmd says that it's sure to be evaluated,
                          -- so we'll end up case-binding it
           ; (app, floats) <- rebuild_app env args fun' fun_floats []
           ; mb_saturate Nothing app floats depth }

    -- Saturate if necessary
    mb_saturate head app floats depth =
       case head of
         Just fn_id -> do { sat_app <- maybeSaturate fn_id app depth
                          ; return (floats, sat_app) }
         _other              -> return (floats, app)

    -- Deconstruct and rebuild the application, floating any non-atomic
    -- arguments to the outside.  We collect the type of the expression,
    -- the head of the application, and the number of actual value arguments,
    -- all of which are used to possibly saturate this application if it
    -- has a constructor or primop at the head.
    rebuild_app
        :: CorePrepEnv
        -> [ArgInfo]                  -- The arguments (inner to outer)
        -> CpeApp
        -> Floats
        -> [Demand]
        -> UniqSM (CpeApp, Floats)
    rebuild_app _ [] app floats ss
      = assert (null ss) -- make sure we used all the strictness info
        return (app, floats)

    rebuild_app env (a : as) fun' floats ss = case a of

      CpeApp (Type arg_ty)
        -> rebuild_app env as (App fun' (Type arg_ty')) floats ss
        where
          arg_ty' = cpSubstTy env arg_ty

      CpeApp (Coercion co)
        -> rebuild_app env as (App fun' (Coercion co')) floats ss
        where
            co' = cpSubstCo env co

      CpeApp arg -> do
        let (ss1, ss_rest)  -- See Note [lazyId magic] in GHC.Types.Id.Make
               = case (ss, isLazyExpr arg) of
                   (_   : ss_rest, True)  -> (topDmd, ss_rest)
                   (ss1 : ss_rest, False) -> (ss1,    ss_rest)
                   ([],            _)     -> (topDmd, [])
        (fs, arg') <- cpeArg top_env ss1 arg
        rebuild_app env as (App fun' arg') (fs `appendFloats` floats) ss_rest

      CpeCast co
        -> rebuild_app env as (Cast fun' co') floats ss
        where
           co' = cpSubstCo env co

      CpeTick tickish
        -- See [Floating Ticks in CorePrep]
        -> rebuild_app env as fun' (addFloat floats (FloatTick tickish)) ss

isLazyExpr :: CoreExpr -> Bool
-- See Note [lazyId magic] in GHC.Types.Id.Make
isLazyExpr (Cast e _)              = isLazyExpr e
isLazyExpr (Tick _ e)              = isLazyExpr e
isLazyExpr (Var f `App` _ `App` _) = f `hasKey` lazyIdKey
isLazyExpr _                       = False

{- Note [runRW magic]
~~~~~~~~~~~~~~~~~~~~~
Some definitions, for instance @runST@, must have careful control over float out
of the bindings in their body. Consider this use of @runST@,

    f x = runST ( \ s -> let (a, s')  = newArray# 100 [] s
                             (_, s'') = fill_in_array_or_something a x s'
                         in freezeArray# a s'' )

If we inline @runST@, we'll get:

    f x = let (a, s')  = newArray# 100 [] realWorld#{-NB-}
              (_, s'') = fill_in_array_or_something a x s'
          in freezeArray# a s''

And now if we allow the @newArray#@ binding to float out to become a CAF,
we end up with a result that is totally and utterly wrong:

    f = let (a, s')  = newArray# 100 [] realWorld#{-NB-} -- YIKES!!!
        in \ x ->
            let (_, s'') = fill_in_array_or_something a x s'
            in freezeArray# a s''

All calls to @f@ will share a {\em single} array! Clearly this is nonsense and
must be prevented.

This is what @runRW#@ gives us: by being inlined extremely late in the
optimization (right before lowering to STG, in CorePrep), we can ensure that
no further floating will occur. This allows us to safely inline things like
@runST@, which are otherwise needlessly expensive (see #10678 and #5916).

'runRW' has a variety of quirks:

 * 'runRW' is known-key with a NOINLINE definition in
   GHC.Magic. This definition is used in cases where runRW is curried.

 * In addition to its normal Haskell definition in GHC.Magic, we give it
   a special late inlining here in CorePrep and GHC.StgToByteCode, avoiding
   the incorrect sharing due to float-out noted above.

 * It is levity-polymorphic:

    runRW# :: forall (r1 :: RuntimeRep). (o :: TYPE r)
           => (State# RealWorld -> (# State# RealWorld, o #))
           -> (# State# RealWorld, o #)

 * It has some special simplification logic to allow unboxing of results when
   runRW# appears in a strict context. See Note [Simplification of runRW#]
   below.

 * Since its body is inlined, we allow runRW#'s argument to contain jumps to
   join points. That is, the following is allowed:

    join j x = ...
    in runRW# @_ @_ (\s -> ... jump j 42 ...)

   The Core Linter knows about this. See Note [Linting of runRW#] in
   GHC.Core.Lint for details.

   The occurrence analyser and SetLevels also know about this, as described in
   Note [Simplification of runRW#].

Other relevant Notes:

 * Note [Simplification of runRW#] below, describing a transformation of runRW
   applications in strict contexts performed by the simplifier.
 * Note [Linting of runRW#] in GHC.Core.Lint
 * Note [runRW arg] below, describing a non-obvious case where the
   late-inlining could go wrong.


 Note [runRW arg]
~~~~~~~~~~~~~~~~~~~
Consider the Core program (from #11291),

   runRW# (case bot of {})

The late inlining logic in cpe_app would transform this into:

   (case bot of {}) realWorldPrimId#

Which would rise to a panic in CoreToStg.myCollectArgs, which expects only
variables in function position.

However, as runRW#'s strictness signature captures the fact that it will call
its argument this can't happen: the simplifier will transform the bottoming
application into simply (case bot of {}).

Note that this reasoning does *not* apply to non-bottoming continuations like:

    hello :: Bool -> Int
    hello n =
      runRW# (
          case n of
            True -> \s -> 23
            _    -> \s -> 10)

Why? The difference is that (case bot of {}) is considered by okCpeArg to be
trivial, consequently cpeArg (which the catch-all case of cpe_app calls on both
the function and the arguments) will forgo binding it to a variable. By
contrast, in the non-bottoming case of `hello` above  the function will be
deemed non-trivial and consequently will be case-bound.


Note [Simplification of runRW#]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the program,

    case runRW# (\s -> I# 42#) of
      I# n# -> f n#

There is no reason why we should allocate an I# constructor given that we
immediately destructure it.

To avoid this the simplifier has a special transformation rule, specific to
runRW#, that pushes a strict context into runRW#'s continuation.  See the
`runRW#` guard in `GHC.Core.Opt.Simplify.rebuildCall`.  That is, it transforms

    K[ runRW# @r @ty cont ]
              ~>
    runRW# @r @ty (\s -> K[cont s])

This has a few interesting implications. Consider, for instance, this program:

    join j = ...
    in case runRW# @r @ty cont of
         result -> jump j result

Performing the transform described above would result in:

    join j x = ...
    in runRW# @r @ty (\s ->
         case cont of in
           result -> jump j result
       )

If runRW# were a "normal" function this call to join point j would not be
allowed in its continuation argument. However, since runRW# is inlined (as
described in Note [runRW magic] above), such join point occurrences are
completely fine. Both occurrence analysis (see the runRW guard in occAnalApp)
and Core Lint (see the App case of lintCoreExpr) have special treatment for
runRW# applications. See Note [Linting of runRW#] for details on the latter.

Moreover, it's helpful to ensure that runRW's continuation isn't floated out
For instance, if we have

    runRW# (\s -> do_something)

where do_something contains only top-level free variables, we may be tempted to
float the argument to the top-level. However, we must resist this urge as since
doing so would then require that runRW# produce an allocation and call, e.g.:

    let lvl = \s -> do_somethign
    in
    ....(runRW# lvl)....

whereas without floating the inlining of the definition of runRW would result
in straight-line code. Consequently, GHC.Core.Opt.SetLevels.lvlApp has special
treatment for runRW# applications, ensure the arguments are not floated as
MFEs.

Now that we float evaluation context into runRW#, we also have to give runRW# a
special higher-order CPR transformer lest we risk #19822. E.g.,

  case runRW# (\s -> doThings) of x -> Data.Text.Text x something something'
      ~>
  runRW# (\s -> case doThings s of x -> Data.Text.Text x something something')

The former had the CPR property, and so should the latter.

Other considered designs
------------------------

One design that was rejected was to *require* that runRW#'s continuation be
headed by a lambda. However, this proved to be quite fragile. For instance,
SetLevels is very eager to float bottoming expressions. For instance given
something of the form,

    runRW# @r @ty (\s -> case expr of x -> undefined)

SetLevels will see that the body the lambda is bottoming and will consequently
float it to the top-level (assuming expr has no free coercion variables which
prevent this). We therefore end up with

    runRW# @r @ty (\s -> lvl s)

Which the simplifier will beta reduce, leaving us with

    runRW# @r @ty lvl

Breaking our desired invariant. Ultimately we decided to simply accept that
the continuation may not be a manifest lambda.


-- ---------------------------------------------------------------------------
--      CpeArg: produces a result satisfying CpeArg
-- ---------------------------------------------------------------------------

Note [ANF-ising literal string arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Consider a program like,

    data Foo = Foo Addr#

    foo = Foo "turtle"#

When we go to ANFise this we might think that we want to float the string
literal like we do any other non-trivial argument. This would look like,

    foo = u\ [] case "turtle"# of s { __DEFAULT__ -> Foo s }

However, this 1) isn't necessary since strings are in a sense "trivial"; and 2)
wreaks havoc on the CAF annotations that we produce here since we the result
above is caffy since it is updateable. Ideally at some point in the future we
would like to just float the literal to the top level as suggested in #11312,

    s = "turtle"#
    foo = Foo s

However, until then we simply add a special case excluding literals from the
floating done by cpeArg.
-}

-- | Is an argument okay to CPE?
okCpeArg :: CoreExpr -> Bool
-- Don't float literals. See Note [ANF-ising literal string arguments].
okCpeArg (Lit _) = False
-- Do not eta expand a trivial argument
okCpeArg expr    = not (exprIsTrivial expr)

-- This is where we arrange that a non-trivial argument is let-bound
cpeArg :: CorePrepEnv -> Demand
       -> CoreArg -> UniqSM (Floats, CpeArg)
cpeArg env dmd arg
  = do { (floats1, arg1) <- cpeRhsE env arg     -- arg1 can be a lambda
       ; let arg_ty      = exprType arg1
             is_unlifted = isUnliftedType arg_ty
             want_float  = wantFloatNested NonRecursive dmd is_unlifted
       ; (floats2, arg2) <- if want_float floats1 arg1
                            then return (floats1, arg1)
                            else dontFloat floats1 arg1
                -- Else case: arg1 might have lambdas, and we can't
                --            put them inside a wrapBinds

       ; if okCpeArg arg2
         then do { v <- newVar arg_ty
                 ; let arg3      = cpeEtaExpand (exprArity arg2) arg2
                       arg_float = mkFloat dmd is_unlifted v arg3
                 ; return (addFloat floats2 arg_float, varToCoreExpr v) }
         else return (floats2, arg2)
       }

{-
Note [Floating unlifted arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider    C (let v* = expensive in v)

where the "*" indicates "will be demanded".  Usually v will have been
inlined by now, but let's suppose it hasn't (see #2756).  Then we
do *not* want to get

     let v* = expensive in C v

because that has different strictness.  Hence the use of 'allLazy'.
(NB: the let v* turns into a FloatCase, in mkLocalNonRec.)


------------------------------------------------------------------------------
-- Building the saturated syntax
-- ---------------------------------------------------------------------------

Note [Eta expansion of hasNoBinding things in CorePrep]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
maybeSaturate deals with eta expanding to saturate things that can't deal with
unsaturated applications (identified by 'hasNoBinding', currently just
foreign calls and unboxed tuple/sum constructors).

Historical Note: Note that eta expansion in CorePrep used to be very fragile
due to the "prediction" of CAFfyness that we used to make during tidying.
We previously saturated primop
applications here as well but due to this fragility (see #16846) we now deal
with this another way, as described in Note [Primop wrappers] in GHC.Builtin.PrimOps.
-}

maybeSaturate :: Id -> CpeApp -> Int -> UniqSM CpeRhs
maybeSaturate fn expr n_args
  | hasNoBinding fn        -- There's no binding
  = return sat_expr

  | otherwise
  = return expr
  where
    fn_arity     = idArity fn
    excess_arity = fn_arity - n_args
    sat_expr     = cpeEtaExpand excess_arity expr

{-
************************************************************************
*                                                                      *
                Simple GHC.Core operations
*                                                                      *
************************************************************************
-}

{-
-- -----------------------------------------------------------------------------
--      Eta reduction
-- -----------------------------------------------------------------------------

Note [Eta expansion]
~~~~~~~~~~~~~~~~~~~~~
Eta expand to match the arity claimed by the binder Remember,
CorePrep must not change arity

Eta expansion might not have happened already, because it is done by
the simplifier only when there at least one lambda already.

NB1:we could refrain when the RHS is trivial (which can happen
    for exported things).  This would reduce the amount of code
    generated (a little) and make things a little words for
    code compiled without -O.  The case in point is data constructor
    wrappers.

NB2: we have to be careful that the result of etaExpand doesn't
   invalidate any of the assumptions that CorePrep is attempting
   to establish.  One possible cause is eta expanding inside of
   an SCC note - we're now careful in etaExpand to make sure the
   SCC is pushed inside any new lambdas that are generated.

Note [Eta expansion and the CorePrep invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It turns out to be much much easier to do eta expansion
*after* the main CorePrep stuff.  But that places constraints
on the eta expander: given a CpeRhs, it must return a CpeRhs.

For example here is what we do not want:
                f = /\a -> g (h 3)      -- h has arity 2
After ANFing we get
                f = /\a -> let s = h 3 in g s
and now we do NOT want eta expansion to give
                f = /\a -> \ y -> (let s = h 3 in g s) y

Instead GHC.Core.Opt.Arity.etaExpand gives
                f = /\a -> \y -> let s = h 3 in g s y

-}

cpeEtaExpand :: Arity -> CpeRhs -> CpeRhs
cpeEtaExpand arity expr
  | arity == 0 = expr
  | otherwise  = etaExpand arity expr

{-
-- -----------------------------------------------------------------------------
--      Eta reduction
-- -----------------------------------------------------------------------------

Why try eta reduction?  Hasn't the simplifier already done eta?
But the simplifier only eta reduces if that leaves something
trivial (like f, or f Int).  But for deLam it would be enough to
get to a partial application:
        case x of { p -> \xs. map f xs }
    ==> case x of { p -> map f }
-}

-- When updating this function, make sure it lines up with
-- GHC.Core.Utils.tryEtaReduce!
tryEtaReducePrep :: [CoreBndr] -> CoreExpr -> Maybe CoreExpr
tryEtaReducePrep bndrs expr@(App _ _)
  | ok_to_eta_reduce f
  , n_remaining >= 0
  , and (zipWith ok bndrs last_args)
  , not (any (`elemVarSet` fvs_remaining) bndrs)
  , exprIsHNF remaining_expr   -- Don't turn value into a non-value
                               -- else the behaviour with 'seq' changes
  = Just remaining_expr
  where
    (f, args) = collectArgs expr
    remaining_expr = mkApps f remaining_args
    fvs_remaining = exprFreeVars remaining_expr
    (remaining_args, last_args) = splitAt n_remaining args
    n_remaining = length args - length bndrs

    ok bndr (Var arg) = bndr == arg
    ok _    _         = False

    -- We can't eta reduce something which must be saturated.
    ok_to_eta_reduce (Var f) =  not (hasNoBinding f) &&
                                not (isLinearType (idType f)) && -- Unsure why this is unsafe.
                                (not (isJoinId f) || idJoinArity f <= n_remaining)
                                -- Don't undersaturate join points.
                                -- See Note [Invariants on join points] in GHC.Core, and #20599


    ok_to_eta_reduce _       = False -- Safe. ToDo: generalise


tryEtaReducePrep bndrs (Tick tickish e)
  | tickishFloatable tickish
  = fmap (mkTick tickish) $ tryEtaReducePrep bndrs e

tryEtaReducePrep _ _ = Nothing

{-
************************************************************************
*                                                                      *
                Floats
*                                                                      *
************************************************************************

Note [Pin demand info on floats]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We pin demand info on floated lets, so that we can see the one-shot thunks.

Note [Speculative evaluation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Since call-by-value is much cheaper than call-by-need, we case-bind arguments
that are either

  1. Strictly evaluated anyway, according to the DmdSig of the callee, or
  2. ok-for-spec, according to 'exprOkForSpeculation'

While (1) is a no-brainer and always beneficial, (2) is a bit
more subtle, as the careful haddock for 'exprOkForSpeculation'
points out. Still, by case-binding the argument we don't need
to allocate a thunk for it, whose closure must be retained as
long as the callee might evaluate it. And if it is evaluated on
most code paths anyway, we get to turn the unknown eval in the
callee into a known call at the call site.
-}

data FloatingBind
  = FloatLet CoreBind    -- Rhs of bindings are CpeRhss
                         -- They are always of lifted type;
                         -- unlifted ones are done with FloatCase

 | FloatCase
      CpeBody         -- Always ok-for-speculation
      Id              -- Case binder
      AltCon [Var]    -- Single alternative
      Bool            -- Ok-for-speculation; False of a strict,
                      -- but lifted binding

 -- | See Note [Floating Ticks in CorePrep]
 | FloatTick CoreTickish

data Floats = Floats OkToSpec (OrdList FloatingBind)

instance Outputable FloatingBind where
  ppr (FloatLet b) = ppr b
  ppr (FloatCase r b k bs ok) = text "case" <> braces (ppr ok) <+> ppr r
                                <+> text "of"<+> ppr b <> text "@"
                                <> case bs of
                                   [] -> ppr k
                                   _  -> parens (ppr k <+> ppr bs)
  ppr (FloatTick t) = ppr t

instance Outputable Floats where
  ppr (Floats flag fs) = text "Floats" <> brackets (ppr flag) <+>
                         braces (vcat (map ppr (fromOL fs)))

instance Outputable OkToSpec where
  ppr OkToSpec    = text "OkToSpec"
  ppr IfUnboxedOk = text "IfUnboxedOk"
  ppr NotOkToSpec = text "NotOkToSpec"

-- Can we float these binds out of the rhs of a let?  We cache this decision
-- to avoid having to recompute it in a non-linear way when there are
-- deeply nested lets.
data OkToSpec
   = OkToSpec           -- Lazy bindings of lifted type
   | IfUnboxedOk        -- A mixture of lazy lifted bindings and n
                        -- ok-to-speculate unlifted bindings
   | NotOkToSpec        -- Some not-ok-to-speculate unlifted bindings

mkFloat :: Demand -> Bool -> Id -> CpeRhs -> FloatingBind
mkFloat dmd is_unlifted bndr rhs
  | is_strict || ok_for_spec -- See Note [Speculative evaluation]
  , not is_hnf  = FloatCase rhs bndr DEFAULT [] ok_for_spec
    -- Don't make a case for a HNF binding, even if it's strict
    -- Otherwise we get  case (\x -> e) of ...!

  | is_unlifted = FloatCase rhs bndr DEFAULT [] True
      -- we used to assertPpr ok_for_spec (ppr rhs) here, but it is now disabled
      -- because exprOkForSpeculation isn't stable under ANF-ing. See for
      -- example #19489 where the following unlifted expression:
      --
      --    GHC.Prim.(#|_#) @LiftedRep @LiftedRep @[a_ax0] @[a_ax0]
      --                    (GHC.Types.: @a_ax0 a2_agq a3_agl)
      --
      -- is ok-for-spec but is ANF-ised into:
      --
      --    let sat = GHC.Types.: @a_ax0 a2_agq a3_agl
      --    in GHC.Prim.(#|_#) @LiftedRep @LiftedRep @[a_ax0] @[a_ax0] sat
      --
      -- which isn't ok-for-spec because of the let-expression.

  | is_hnf      = FloatLet (NonRec bndr                       rhs)
  | otherwise   = FloatLet (NonRec (setIdDemandInfo bndr dmd) rhs)
                   -- See Note [Pin demand info on floats]
  where
    is_hnf      = exprIsHNF rhs
    is_strict   = isStrUsedDmd dmd
    ok_for_spec = exprOkForSpeculation rhs

emptyFloats :: Floats
emptyFloats = Floats OkToSpec nilOL

isEmptyFloats :: Floats -> Bool
isEmptyFloats (Floats _ bs) = isNilOL bs

wrapBinds :: Floats -> CpeBody -> CpeBody
wrapBinds (Floats _ binds) body
  = foldrOL mk_bind body binds
  where
    mk_bind (FloatCase rhs bndr con bs _) body = Case rhs bndr (exprType body) [Alt con bs body]
    mk_bind (FloatLet bind)               body = Let bind body
    mk_bind (FloatTick tickish)           body = mkTick tickish body

addFloat :: Floats -> FloatingBind -> Floats
addFloat (Floats ok_to_spec floats) new_float
  = Floats (combine ok_to_spec (check new_float)) (floats `snocOL` new_float)
  where
    check (FloatLet {})  = OkToSpec
    check (FloatCase _ _ _ _ ok_for_spec)
      | ok_for_spec = IfUnboxedOk
      | otherwise   = NotOkToSpec
    check FloatTick{}    = OkToSpec
        -- The ok-for-speculation flag says that it's safe to
        -- float this Case out of a let, and thereby do it more eagerly
        -- We need the top-level flag because it's never ok to float
        -- an unboxed binding to the top level

unitFloat :: FloatingBind -> Floats
unitFloat = addFloat emptyFloats

appendFloats :: Floats -> Floats -> Floats
appendFloats (Floats spec1 floats1) (Floats spec2 floats2)
  = Floats (combine spec1 spec2) (floats1 `appOL` floats2)

concatFloats :: [Floats] -> OrdList FloatingBind
concatFloats = foldr (\ (Floats _ bs1) bs2 -> appOL bs1 bs2) nilOL

combine :: OkToSpec -> OkToSpec -> OkToSpec
combine NotOkToSpec _ = NotOkToSpec
combine _ NotOkToSpec = NotOkToSpec
combine IfUnboxedOk _ = IfUnboxedOk
combine _ IfUnboxedOk = IfUnboxedOk
combine _ _           = OkToSpec

deFloatTop :: Floats -> [CoreBind]
-- For top level only; we don't expect any FloatCases
deFloatTop (Floats _ floats)
  = foldrOL get [] floats
  where
    get (FloatLet b)               bs = get_bind b                 : bs
    get (FloatCase body var _ _ _) bs = get_bind (NonRec var body) : bs
    get b _ = pprPanic "corePrepPgm" (ppr b)

    -- See Note [Dead code in CorePrep]
    get_bind (NonRec x e) = NonRec x (occurAnalyseExpr e)
    get_bind (Rec xes)    = Rec [(x, occurAnalyseExpr e) | (x, e) <- xes]

---------------------------------------------------------------------------

canFloat :: Floats -> CpeRhs -> Maybe (Floats, CpeRhs)
canFloat (Floats ok_to_spec fs) rhs
  | OkToSpec <- ok_to_spec           -- Worth trying
  , Just fs' <- go nilOL (fromOL fs)
  = Just (Floats OkToSpec fs', rhs)
  | otherwise
  = Nothing
  where
    go :: OrdList FloatingBind -> [FloatingBind]
       -> Maybe (OrdList FloatingBind)

    go (fbs_out) [] = Just fbs_out

    go fbs_out (fb@(FloatLet _) : fbs_in)
      = go (fbs_out `snocOL` fb) fbs_in

    go fbs_out (ft@FloatTick{} : fbs_in)
      = go (fbs_out `snocOL` ft) fbs_in

    go _ (FloatCase{} : _) = Nothing


wantFloatNested :: RecFlag -> Demand -> Bool -> Floats -> CpeRhs -> Bool
wantFloatNested is_rec dmd is_unlifted floats rhs
  =  isEmptyFloats floats
  || isStrUsedDmd dmd
  || is_unlifted
  || (allLazyNested is_rec floats && exprIsHNF rhs)
        -- Why the test for allLazyNested?
        --      v = f (x `divInt#` y)
        -- we don't want to float the case, even if f has arity 2,
        -- because floating the case would make it evaluated too early

allLazyTop :: Floats -> Bool
allLazyTop (Floats OkToSpec _) = True
allLazyTop _                   = False

allLazyNested :: RecFlag -> Floats -> Bool
allLazyNested _      (Floats OkToSpec    _) = True
allLazyNested _      (Floats NotOkToSpec _) = False
allLazyNested is_rec (Floats IfUnboxedOk _) = isNonRec is_rec

{-
************************************************************************
*                                                                      *
                Cloning
*                                                                      *
************************************************************************
-}

-- ---------------------------------------------------------------------------
--                      The environment
-- ---------------------------------------------------------------------------

{- Note [Inlining in CorePrep]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
There is a subtle but important invariant that must be upheld in the output
of CorePrep: there are no "trivial" updatable thunks.  Thus, this Core
is impermissible:

     let x :: ()
         x = y

(where y is a reference to a GLOBAL variable).  Thunks like this are silly:
they can always be profitably replaced by inlining x with y. Consequently,
the code generator/runtime does not bother implementing this properly
(specifically, there is no implementation of stg_ap_0_upd_info, which is the
stack frame that would be used to update this thunk.  The "0" means it has
zero free variables.)

In general, the inliner is good at eliminating these let-bindings.  However,
there is one case where these trivial updatable thunks can arise: when
we are optimizing away 'lazy' (see Note [lazyId magic], and also
'cpeRhsE'.)  Then, we could have started with:

     let x :: ()
         x = lazy @ () y

which is a perfectly fine, non-trivial thunk, but then CorePrep will
drop 'lazy', giving us 'x = y' which is trivial and impermissible.
The solution is CorePrep to have a miniature inlining pass which deals
with cases like this.  We can then drop the let-binding altogether.

Why does the removal of 'lazy' have to occur in CorePrep?
The gory details are in Note [lazyId magic] in GHC.Types.Id.Make, but the
main reason is that lazy must appear in unfoldings (optimizer
output) and it must prevent call-by-value for catch# (which
is implemented by CorePrep.)

An alternate strategy for solving this problem is to have the
inliner treat 'lazy e' as a trivial expression if 'e' is trivial.
We decided not to adopt this solution to keep the definition
of 'exprIsTrivial' simple.

There is ONE caveat however: for top-level bindings we have
to preserve the binding so that we float the (hacky) non-recursive
binding for data constructors; see Note [Data constructor workers].

Note [CorePrep inlines trivial CoreExpr not Id]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Why does cpe_env need to be an IdEnv CoreExpr, as opposed to an
IdEnv Id?  Naively, we might conjecture that trivial updatable thunks
as per Note [Inlining in CorePrep] always have the form
'lazy @ SomeType gbl_id'.  But this is not true: the following is
perfectly reasonable Core:

     let x :: ()
         x = lazy @ (forall a. a) y @ Bool

When we inline 'x' after eliminating 'lazy', we need to replace
occurrences of 'x' with 'y @ bool', not just 'y'.  Situations like
this can easily arise with higher-rank types; thus, cpe_env must
map to CoreExprs, not Ids.

-}

data CorePrepEnv
  = CPE { cpe_dynFlags        :: DynFlags
        , cpe_env             :: IdEnv CoreExpr   -- Clone local Ids
        -- ^ This environment is used for three operations:
        --
        --      1. To support cloning of local Ids so that they are
        --      all unique (see item (6) of CorePrep overview).
        --
        --      2. To support beta-reduction of runRW, see
        --      Note [runRW magic] and Note [runRW arg].
        --
        --      3. To let us inline trivial RHSs of non top-level let-bindings,
        --      see Note [lazyId magic], Note [Inlining in CorePrep]
        --      and Note [CorePrep inlines trivial CoreExpr not Id] (#12076)

        , cpe_tyco_env :: Maybe CpeTyCoEnv -- See Note [CpeTyCoEnv]

        , cpe_convertNumLit   :: LitNumType -> Integer -> Maybe CoreExpr
        -- ^ Convert some numeric literals (Integer, Natural) into their
        -- final Core form
    }

mkInitialCorePrepEnv :: HscEnv -> IO CorePrepEnv
mkInitialCorePrepEnv hsc_env = do
   convertNumLit <- mkConvertNumLiteral hsc_env
   return $ CPE
      { cpe_dynFlags      = hsc_dflags hsc_env
      , cpe_env           = emptyVarEnv
      , cpe_tyco_env      = Nothing
      , cpe_convertNumLit = convertNumLit
      }

extendCorePrepEnv :: CorePrepEnv -> Id -> Id -> CorePrepEnv
extendCorePrepEnv cpe id id'
    = cpe { cpe_env = extendVarEnv (cpe_env cpe) id (Var id') }

extendCorePrepEnvExpr :: CorePrepEnv -> Id -> CoreExpr -> CorePrepEnv
extendCorePrepEnvExpr cpe id expr
    = cpe { cpe_env = extendVarEnv (cpe_env cpe) id expr }

extendCorePrepEnvList :: CorePrepEnv -> [(Id,Id)] -> CorePrepEnv
extendCorePrepEnvList cpe prs
    = cpe { cpe_env = extendVarEnvList (cpe_env cpe)
                        (map (\(id, id') -> (id, Var id')) prs) }

lookupCorePrepEnv :: CorePrepEnv -> Id -> CoreExpr
lookupCorePrepEnv cpe id
  = case lookupVarEnv (cpe_env cpe) id of
        Nothing  -> Var id
        Just exp -> exp

------------------------------------------------------------------------------
--           CpeTyCoEnv
-- ---------------------------------------------------------------------------

{- Note [CpeTyCoEnv]
~~~~~~~~~~~~~~~~~~~~
The cpe_tyco_env :: Maybe CpeTyCoEnv field carries a substitution
for type and coercion varibles

* We need the coercion substitution to support the elimination of
  unsafeEqualityProof (see Note [Unsafe coercions])

* We need the type substitution in case one of those unsafe
  coercions occurs in the kind of tyvar binder (sigh)

We don't need an in-scope set because we don't clone any of these
binders at all, so no new capture can take place.

The cpe_tyco_env is almost always empty -- it only gets populated
when we get under an usafeEqualityProof.  Hence the Maybe CpeTyCoEnv,
which makes everything into a no-op in the common case.
-}

data CpeTyCoEnv = TCE TvSubstEnv CvSubstEnv

emptyTCE :: CpeTyCoEnv
emptyTCE = TCE emptyTvSubstEnv emptyCvSubstEnv

extend_tce_cv :: CpeTyCoEnv -> CoVar -> Coercion -> CpeTyCoEnv
extend_tce_cv (TCE tv_env cv_env) cv co
  = TCE tv_env (extendVarEnv cv_env cv co)

extend_tce_tv :: CpeTyCoEnv -> TyVar -> Type -> CpeTyCoEnv
extend_tce_tv (TCE tv_env cv_env) tv ty
  = TCE (extendVarEnv tv_env tv ty) cv_env

lookup_tce_cv :: CpeTyCoEnv -> CoVar -> Coercion
lookup_tce_cv (TCE _ cv_env) cv
  = case lookupVarEnv cv_env cv of
        Just co -> co
        Nothing -> mkCoVarCo cv

lookup_tce_tv :: CpeTyCoEnv -> TyVar -> Type
lookup_tce_tv (TCE tv_env _) tv
  = case lookupVarEnv tv_env tv of
        Just ty -> ty
        Nothing -> mkTyVarTy tv

extendCoVarEnv :: CorePrepEnv -> CoVar -> Coercion -> CorePrepEnv
extendCoVarEnv cpe@(CPE { cpe_tyco_env = mb_tce }) cv co
  = cpe { cpe_tyco_env = Just (extend_tce_cv tce cv co) }
  where
    tce = mb_tce `orElse` emptyTCE


cpSubstTy :: CorePrepEnv -> Type -> Type
cpSubstTy (CPE { cpe_tyco_env = mb_env }) ty
  = case mb_env of
      Just env -> runIdentity (subst_ty env ty)
      Nothing  -> ty

cpSubstCo :: CorePrepEnv -> Coercion -> Coercion
cpSubstCo (CPE { cpe_tyco_env = mb_env }) co
  = case mb_env of
      Just tce -> runIdentity (subst_co tce co)
      Nothing  -> co

subst_tyco_mapper :: TyCoMapper CpeTyCoEnv Identity
subst_tyco_mapper = TyCoMapper
  { tcm_tyvar      = \env tv -> return (lookup_tce_tv env tv)
  , tcm_covar      = \env cv -> return (lookup_tce_cv env cv)
  , tcm_hole       = \_ hole -> pprPanic "subst_co_mapper:hole" (ppr hole)
  , tcm_tycobinder = \env tcv _vis -> if isTyVar tcv
                                      then return (subst_tv_bndr env tcv)
                                      else return (subst_cv_bndr env tcv)
  , tcm_tycon      = \tc -> return tc }

subst_ty :: CpeTyCoEnv -> Type     -> Identity Type
subst_co :: CpeTyCoEnv -> Coercion -> Identity Coercion
(subst_ty, _, subst_co, _) = mapTyCoX subst_tyco_mapper

cpSubstTyVarBndr :: CorePrepEnv -> TyVar -> (CorePrepEnv, TyVar)
cpSubstTyVarBndr env@(CPE { cpe_tyco_env = mb_env }) tv
  = case mb_env of
      Nothing  -> (env, tv)
      Just tce -> (env { cpe_tyco_env = Just tce' }, tv')
               where
                  (tce', tv') = subst_tv_bndr tce tv

subst_tv_bndr :: CpeTyCoEnv -> TyVar -> (CpeTyCoEnv, TyVar)
subst_tv_bndr tce tv
  = (extend_tce_tv tce tv (mkTyVarTy tv'), tv')
  where
    tv'   = mkTyVar (tyVarName tv) kind'
    kind' = runIdentity $ subst_ty tce $ tyVarKind tv

cpSubstCoVarBndr :: CorePrepEnv -> CoVar -> (CorePrepEnv, CoVar)
cpSubstCoVarBndr env@(CPE { cpe_tyco_env = mb_env }) cv
  = case mb_env of
      Nothing  -> (env, cv)
      Just tce -> (env { cpe_tyco_env = Just tce' }, cv')
               where
                  (tce', cv') = subst_cv_bndr tce cv

subst_cv_bndr :: CpeTyCoEnv -> CoVar -> (CpeTyCoEnv, CoVar)
subst_cv_bndr tce cv
  = (extend_tce_cv tce cv (mkCoVarCo cv'), cv')
  where
    cv' = mkCoVar (varName cv) ty'
    ty' = runIdentity (subst_ty tce $ varType cv)

------------------------------------------------------------------------------
-- Cloning binders
-- ---------------------------------------------------------------------------

cpCloneBndrs :: CorePrepEnv -> [InVar] -> UniqSM (CorePrepEnv, [OutVar])
cpCloneBndrs env bs = mapAccumLM cpCloneBndr env bs

cpCloneBndr  :: CorePrepEnv -> InVar -> UniqSM (CorePrepEnv, OutVar)
cpCloneBndr env bndr
  | isTyVar bndr
  = return (cpSubstTyVarBndr env bndr)

  | isCoVar bndr
  = return (cpSubstCoVarBndr env bndr)

  | otherwise
  = do { bndr' <- clone_it bndr

       -- Drop (now-useless) rules/unfoldings
       -- See Note [Drop unfoldings and rules]
       -- and Note [Preserve evaluatedness] in GHC.Core.Tidy
       ; let unfolding' = zapUnfolding (realIdUnfolding bndr)
                          -- Simplifier will set the Id's unfolding

             bndr'' = bndr' `setIdUnfolding`      unfolding'
                            `setIdSpecialisation` emptyRuleInfo

       ; return (extendCorePrepEnv env bndr bndr'', bndr'') }
  where
    clone_it bndr
      | isLocalId bndr
      = do { uniq <- getUniqueM
           ; let ty' = cpSubstTy env (idType bndr)
           ; return (setVarUnique (setIdType bndr ty') uniq) }

      | otherwise   -- Top level things, which we don't want
                    -- to clone, have become GlobalIds by now
      = return bndr

{- Note [Drop unfoldings and rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to drop the unfolding/rules on every Id:

  - We are now past interface-file generation, and in the
    codegen pipeline, so we really don't need full unfoldings/rules

  - The unfolding/rule may be keeping stuff alive that we'd like
    to discard.  See  Note [Dead code in CorePrep]

  - Getting rid of unnecessary unfoldings reduces heap usage

  - We are changing uniques, so if we didn't discard unfoldings/rules
    we'd have to substitute in them

HOWEVER, we want to preserve evaluated-ness;
see Note [Preserve evaluatedness] in GHC.Core.Tidy.
-}

------------------------------------------------------------------------------
-- Cloning ccall Ids; each must have a unique name,
-- to give the code generator a handle to hang it on
-- ---------------------------------------------------------------------------

fiddleCCall :: Id -> UniqSM Id
fiddleCCall id
  | isFCallId id = (id `setVarUnique`) <$> getUniqueM
  | otherwise    = return id

------------------------------------------------------------------------------
-- Generating new binders
-- ---------------------------------------------------------------------------

newVar :: Type -> UniqSM Id
newVar ty
 = seqType ty `seq` do
     uniq <- getUniqueM
     return (mkSysLocalOrCoVar (fsLit "sat") uniq Many ty)


------------------------------------------------------------------------------
-- Floating ticks
-- ---------------------------------------------------------------------------
--
-- Note [Floating Ticks in CorePrep]
--
-- It might seem counter-intuitive to float ticks by default, given
-- that we don't actually want to move them if we can help it. On the
-- other hand, nothing gets very far in CorePrep anyway, and we want
-- to preserve the order of let bindings and tick annotations in
-- relation to each other. For example, if we just wrapped let floats
-- when they pass through ticks, we might end up performing the
-- following transformation:
--
--   src<...> let foo = bar in baz
--   ==>  let foo = src<...> bar in src<...> baz
--
-- Because the let-binding would float through the tick, and then
-- immediately materialize, achieving nothing but decreasing tick
-- accuracy. The only special case is the following scenario:
--
--   let foo = src<...> (let a = b in bar) in baz
--   ==>  let foo = src<...> bar; a = src<...> b in baz
--
-- Here we would not want the source tick to end up covering "baz" and
-- therefore refrain from pushing ticks outside. Instead, we copy them
-- into the floating binds (here "a") in cpePair. Note that where "b"
-- or "bar" are (value) lambdas we have to push the annotations
-- further inside in order to uphold our rules.
--
-- All of this is implemented below in @wrapTicks@.

-- | Like wrapFloats, but only wraps tick floats
wrapTicks :: Floats -> CoreExpr -> (Floats, CoreExpr)
wrapTicks (Floats flag floats0) expr =
    (Floats flag (toOL $ reverse floats1), foldr mkTick expr (reverse ticks1))
  where (floats1, ticks1) = foldlOL go ([], []) $ floats0
        -- Deeply nested constructors will produce long lists of
        -- redundant source note floats here. We need to eliminate
        -- those early, as relying on mkTick to spot it after the fact
        -- can yield O(n^3) complexity [#11095]
        go (floats, ticks) (FloatTick t)
          = assert (tickishPlace t == PlaceNonLam)
            (floats, if any (flip tickishContains t) ticks
                     then ticks else t:ticks)
        go (floats, ticks) f
          = (foldr wrap f (reverse ticks):floats, ticks)

        wrap t (FloatLet bind)           = FloatLet (wrapBind t bind)
        wrap t (FloatCase r b con bs ok) = FloatCase (mkTick t r) b con bs ok
        wrap _ other                     = pprPanic "wrapTicks: unexpected float!"
                                             (ppr other)
        wrapBind t (NonRec binder rhs) = NonRec binder (mkTick t rhs)
        wrapBind t (Rec pairs)         = Rec (mapSnd (mkTick t) pairs)



------------------------------------------------------------------------------
-- Numeric literals
-- ---------------------------------------------------------------------------

-- | Create a function that converts Bignum literals into their final CoreExpr
mkConvertNumLiteral
   :: HscEnv
   -> IO (LitNumType -> Integer -> Maybe CoreExpr)
mkConvertNumLiteral hsc_env = do
   let
      dflags   = hsc_dflags hsc_env
      platform = targetPlatform dflags
      home_unit = hsc_home_unit hsc_env
      guardBignum act
         | isHomeUnitInstanceOf home_unit primUnitId
         = return $ panic "Bignum literals are not supported in ghc-prim"
         | isHomeUnitInstanceOf home_unit bignumUnitId
         = return $ panic "Bignum literals are not supported in ghc-bignum"
         | otherwise = act

      lookupBignumId n      = guardBignum (tyThingId <$> lookupGlobal hsc_env n)

   -- The lookup is done here but the failure (panic) is reported lazily when we
   -- try to access the `bigNatFromWordList` function.
   --
   -- If we ever get built-in ByteArray# literals, we could avoid the lookup by
   -- directly using the Integer/Natural wired-in constructors for big numbers.

   bignatFromWordListId <- lookupBignumId bignatFromWordListName

   let
      convertNumLit nt i = case nt of
         LitNumBigNat  -> Just (convertBignatPrim i)
         _             -> Nothing

      convertBignatPrim i =
         let
            target    = targetPlatform dflags

            -- ByteArray# literals aren't supported (yet). Were they supported,
            -- we would use them directly. We would need to handle
            -- wordSize/endianness conversion between host and target
            -- wordSize  = platformWordSize platform
            -- byteOrder = platformByteOrder platform

            -- For now we build a list of Words and we produce
            -- `bigNatFromWordList# list_of_words`

            words = mkListExpr wordTy (reverse (unfoldr f i))
               where
                  f 0 = Nothing
                  f x = let low  = x .&. mask
                            high = x `shiftR` bits
                        in Just (mkConApp wordDataCon [Lit (mkLitWord platform low)], high)
                  bits = platformWordSizeInBits target
                  mask = 2 ^ bits - 1

         in mkApps (Var bignatFromWordListId) [words]


   return convertNumLit

