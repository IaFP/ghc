{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

Extracting imported and top-level names in scope
-}

{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module GHC.Rename.Names (
        rnImports, getLocalNonValBinders, newRecordSelector,
        extendGlobalRdrEnvRn,
        gresFromAvails,
        calculateAvails,
        reportUnusedNames,
        checkConName,
        mkChildEnv,
        findChildren,
        findImportUsage,
        getMinimalImports,
        printMinimalImports,
        renamePkgQual, renameRawPkgQual,
        ImportDeclUsage
    ) where

import GHC.Prelude

import GHC.Driver.Env
import GHC.Driver.Session
import GHC.Driver.Ppr

import GHC.Rename.Env
import GHC.Rename.Fixity
import GHC.Rename.Utils ( warnUnusedTopBinds, mkFieldEnv )

import GHC.Tc.Errors.Types
import GHC.Tc.Utils.Env
import GHC.Tc.Utils.Monad

import GHC.Hs
import GHC.Iface.Load   ( loadSrcInterface )
import GHC.Iface.Env   ( newGlobalBinder )
import GHC.Builtin.Names
import GHC.Parser.PostProcess ( setRdrNameSpace )
import GHC.Core.Type
import GHC.Core.PatSyn
import GHC.Core.TyCo.Ppr
import GHC.Core.TyCon ( TyCon, tyConName, tyConKind, wF_TC_PREFIX )
import qualified GHC.LanguageExtensions as LangExt

import GHC.Utils.Outputable as Outputable
import GHC.Utils.Misc as Utils
import GHC.Utils.Panic
import GHC.Utils.Trace

import GHC.Types.Fixity.Env
import GHC.Types.SafeHaskell
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Name.Set
import GHC.Types.Name.Reader
import GHC.Types.Avail
import GHC.Types.FieldLabel
import GHC.Types.SourceFile
import GHC.Types.SrcLoc as SrcLoc
import GHC.Types.Basic  ( TopLevelFlag(..) )
import GHC.Types.SourceText
import GHC.Types.Id
import GHC.Types.HpcInfo
import GHC.Types.Unique.FM
import GHC.Types.Error
import GHC.Types.PkgQual

import GHC.Unit
import GHC.Unit.Module.Warnings
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.Imported
import GHC.Unit.Module.Deps
import GHC.Unit.Env

import GHC.Data.Maybe
import GHC.Data.FastString
import GHC.Data.FastString.Env

import Control.Monad
import Data.Either      ( partitionEithers )
import Data.Map         ( Map )
import qualified Data.Map as Map
import Data.Ord         ( comparing )
import Data.List        ( partition, (\\), find, sortBy, groupBy, sortOn )
import Data.Function    ( on )
import qualified Data.Set as S
import System.FilePath  ((</>))

import System.IO
import GHC.Data.Bag

{-
************************************************************************
*                                                                      *
\subsection{rnImports}
*                                                                      *
************************************************************************

Note [Tracking Trust Transitively]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we import a package as well as checking that the direct imports are safe
according to the rules outlined in the Note [Safe Haskell Trust Check] in GHC.Driver.Main
we must also check that these rules hold transitively for all dependent modules
and packages. Doing this without caching any trust information would be very
slow as we would need to touch all packages and interface files a module depends
on. To avoid this we make use of the property that if a modules Safe Haskell
mode changes, this triggers a recompilation from that module in the dependecy
graph. So we can just worry mostly about direct imports.

There is one trust property that can change for a package though without
recompilation being triggered: package trust. So we must check that all
packages a module transitively depends on to be trusted are still trusted when
we are compiling this module (as due to recompilation avoidance some modules
below may not be considered trusted any more without recompilation being
triggered).

We handle this by augmenting the existing transitive list of packages a module M
depends on with a bool for each package that says if it must be trusted when the
module M is being checked for trust. This list of trust required packages for a
single import is gathered in the rnImportDecl function and stored in an
ImportAvails data structure. The union of these trust required packages for all
imports is done by the rnImports function using the combine function which calls
the plusImportAvails function that is a union operation for the ImportAvails
type. This gives us in an ImportAvails structure all packages required to be
trusted for the module we are currently compiling. Checking that these packages
are still trusted (and that direct imports are trusted) is done in
GHC.Driver.Main.checkSafeImports.

See the note below, [Trust Own Package] for a corner case in this method and
how its handled.


Note [Trust Own Package]
~~~~~~~~~~~~~~~~~~~~~~~~
There is a corner case of package trust checking that the usual transitive check
doesn't cover. (For how the usual check operates see the Note [Tracking Trust
Transitively] below). The case is when you import a -XSafe module M and M
imports a -XTrustworthy module N. If N resides in a different package than M,
then the usual check works as M will record a package dependency on N's package
and mark it as required to be trusted. If N resides in the same package as M
though, then importing M should require its own package be trusted due to N
(since M is -XSafe so doesn't create this requirement by itself). The usual
check fails as a module doesn't record a package dependency of its own package.
So instead we now have a bool field in a modules interface file that simply
states if the module requires its own package to be trusted. This field avoids
us having to load all interface files that the module depends on to see if one
is trustworthy.


Note [Trust Transitive Property]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
So there is an interesting design question in regards to transitive trust
checking. Say I have a module B compiled with -XSafe. B is dependent on a bunch
of modules and packages, some packages it requires to be trusted as its using
-XTrustworthy modules from them. Now if I have a module A that doesn't use safe
haskell at all and simply imports B, should A inherit all the trust
requirements from B? Should A now also require that a package p is trusted since
B required it?

We currently say no but saying yes also makes sense. The difference is, if a
module M that doesn't use Safe Haskell imports a module N that does, should all
the trusted package requirements be dropped since M didn't declare that it cares
about Safe Haskell (so -XSafe is more strongly associated with the module doing
the importing) or should it be done still since the author of the module N that
uses Safe Haskell said they cared (so -XSafe is more strongly associated with
the module that was compiled that used it).

Going with yes is a simpler semantics we think and harder for the user to stuff
up but it does mean that Safe Haskell will affect users who don't care about
Safe Haskell as they might grab a package from Cabal which uses safe haskell (say
network) and that packages imports -XTrustworthy modules from another package
(say bytestring), so requires that package is trusted. The user may now get
compilation errors in code that doesn't do anything with Safe Haskell simply
because they are using the network package. They will have to call 'ghc-pkg
trust network' to get everything working. Due to this invasive nature of going
with yes we have gone with no for now.
-}

-- | Process Import Decls.  See 'rnImportDecl' for a description of what
-- the return types represent.
-- Note: Do the non SOURCE ones first, so that we get a helpful warning
-- for SOURCE ones that are unnecessary
rnImports :: [(LImportDecl GhcPs, SDoc)]
          -> RnM ([LImportDecl GhcRn], GlobalRdrEnv, ImportAvails, AnyHpcUsage)
rnImports imports = do
    tcg_env <- getGblEnv
    -- NB: want an identity module here, because it's OK for a signature
    -- module to import from its implementor
    let this_mod = tcg_mod tcg_env
    let (source, ordinary) = partition (is_source_import . fst) imports
        is_source_import d = ideclSource (unLoc d) == IsBoot
    stuff1 <- mapAndReportM (rnImportDecl this_mod) ordinary
    stuff2 <- mapAndReportM (rnImportDecl this_mod) source
    -- Safe Haskell: See Note [Tracking Trust Transitively]
    let (decls, rdr_env, imp_avails, hpc_usage) = combine (stuff1 ++ stuff2)
    -- Update imp_boot_mods if imp_direct_mods mentions any of them
    let merged_import_avail = clobberSourceImports imp_avails
    dflags <- getDynFlags
    let final_import_avail  =
          merged_import_avail { imp_dep_direct_pkgs = S.fromList (implicitPackageDeps dflags)
                                                        `S.union` imp_dep_direct_pkgs merged_import_avail}
    return (decls, rdr_env, final_import_avail, hpc_usage)

  where
    clobberSourceImports imp_avails =
      imp_avails { imp_boot_mods = imp_boot_mods' }
      where
        imp_boot_mods' = mergeUFM combJ id (const mempty)
                            (imp_boot_mods imp_avails)
                            (imp_direct_dep_mods imp_avails)

        combJ (GWIB _ IsBoot) x = Just x
        combJ r _               = Just r
    -- See Note [Combining ImportAvails]
    combine :: [(LImportDecl GhcRn,  GlobalRdrEnv, ImportAvails, AnyHpcUsage)]
            -> ([LImportDecl GhcRn], GlobalRdrEnv, ImportAvails, AnyHpcUsage)
    combine ss =
      let (decls, rdr_env, imp_avails, hpc_usage, finsts) = foldr
            plus
            ([], emptyGlobalRdrEnv, emptyImportAvails, False, emptyModuleSet)
            ss
      in (decls, rdr_env, imp_avails { imp_finsts = moduleSetElts finsts },
            hpc_usage)

    plus (decl,  gbl_env1, imp_avails1, hpc_usage1)
         (decls, gbl_env2, imp_avails2, hpc_usage2, finsts_set)
      = ( decl:decls,
          gbl_env1 `plusGlobalRdrEnv` gbl_env2,
          imp_avails1' `plusImportAvails` imp_avails2,
          hpc_usage1 || hpc_usage2,
          extendModuleSetList finsts_set new_finsts )
      where
      imp_avails1' = imp_avails1 { imp_finsts = [] }
      new_finsts = imp_finsts imp_avails1

{-
Note [Combining ImportAvails]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
imp_finsts in ImportAvails is a list of family instance modules
transitively depended on by an import. imp_finsts for a currently
compiled module is a union of all the imp_finsts of imports.
Computing the union of two lists of size N is O(N^2) and if we
do it to M imports we end up with O(M*N^2). That can get very
expensive for bigger module hierarchies.

Union can be optimized to O(N log N) if we use a Set.
imp_finsts is converted back and forth between dep_finsts, so
changing a type of imp_finsts means either paying for the conversions
or changing the type of dep_finsts as well.

I've measured that the conversions would cost 20% of allocations on my
test case, so that can be ruled out.

Changing the type of dep_finsts forces checkFamInsts to
get the module lists in non-deterministic order. If we wanted to restore
the deterministic order, we'd have to sort there, which is an additional
cost. As far as I can tell, using a non-deterministic order is fine there,
but that's a brittle nonlocal property which I'd like to avoid.

Additionally, dep_finsts is read from an interface file, so its "natural"
type is a list. Which makes it a natural type for imp_finsts.

Since rnImports.combine is really the only place that would benefit from
it being a Set, it makes sense to optimize the hot loop in rnImports.combine
without changing the representation.

So here's what we do: instead of naively merging ImportAvails with
plusImportAvails in a loop, we make plusImportAvails merge empty imp_finsts
and compute the union on the side using Sets. When we're done, we can
convert it back to a list. One nice side effect of this approach is that
if there's a lot of overlap in the imp_finsts of imports, the
Set doesn't really need to grow and we don't need to allocate.

Running generateModules from #14693 with DEPTH=16, WIDTH=30 finishes in
23s before, and 11s after.
-}



-- | Given a located import declaration @decl@ from @this_mod@,
-- calculate the following pieces of information:
--
--  1. An updated 'LImportDecl', where all unresolved 'RdrName' in
--     the entity lists have been resolved into 'Name's,
--
--  2. A 'GlobalRdrEnv' representing the new identifiers that were
--     brought into scope (taking into account module qualification
--     and hiding),
--
--  3. 'ImportAvails' summarizing the identifiers that were imported
--     by this declaration, and
--
--  4. A boolean 'AnyHpcUsage' which is true if the imported module
--     used HPC.
rnImportDecl  :: Module -> (LImportDecl GhcPs, SDoc)
             -> RnM (LImportDecl GhcRn, GlobalRdrEnv, ImportAvails, AnyHpcUsage)
rnImportDecl this_mod
             (L loc decl@(ImportDecl { ideclName = loc_imp_mod_name
                                     , ideclPkgQual = raw_pkg_qual
                                     , ideclSource = want_boot, ideclSafe = mod_safe
                                     , ideclQualified = qual_style, ideclImplicit = implicit
                                     , ideclAs = as_mod, ideclHiding = imp_details }), import_reason)
  = setSrcSpanA loc $ do

    case raw_pkg_qual of
      NoRawPkgQual -> pure ()
      RawPkgQual _ -> do
        pkg_imports <- xoptM LangExt.PackageImports
        when (not pkg_imports) $ addErr packageImportErr

    let qual_only = isImportDeclQualified qual_style

    -- If there's an error in loadInterface, (e.g. interface
    -- file not found) we get lots of spurious errors from 'filterImports'
    let imp_mod_name = unLoc loc_imp_mod_name
        doc = ppr imp_mod_name <+> import_reason

    unit_env <- hsc_unit_env <$> getTopEnv
    let pkg_qual = renameRawPkgQual unit_env raw_pkg_qual

    -- Check for self-import, which confuses the typechecker (#9032)
    -- ghc --make rejects self-import cycles already, but batch-mode may not
    -- at least not until GHC.IfaceToCore.tcHiBootIface, which is too late to avoid
    -- typechecker crashes.  (Indirect self imports are not caught until
    -- GHC.IfaceToCore, see #10337 tracking how to make this error better.)
    --
    -- Originally, we also allowed 'import {-# SOURCE #-} M', but this
    -- caused bug #10182: in one-shot mode, we should never load an hs-boot
    -- file for the module we are compiling into the EPS.  In principle,
    -- it should be possible to support this mode of use, but we would have to
    -- extend Provenance to support a local definition in a qualified location.
    -- For now, we don't support it, but see #10336
    when (imp_mod_name == moduleName this_mod &&
          (case pkg_qual of -- If we have import "<pkg>" M, then we should
                            -- check that "<pkg>" is "this" (which is magic)
                            -- or the name of this_mod's package.  Yurgh!
                            -- c.f. GHC.findModule, and #9997
             NoPkgQual         -> True
             ThisPkg _         -> True
             OtherPkg _        -> False))
         (addErr $ TcRnUnknownMessage $ mkPlainError noHints $
           (text "A module cannot import itself:" <+> ppr imp_mod_name))

    -- Check for a missing import list (Opt_WarnMissingImportList also
    -- checks for T(..) items but that is done in checkDodgyImport below)
    case imp_details of
        Just (False, _) -> return () -- Explicit import list
        _  | implicit   -> return () -- Do not bleat for implicit imports
           | qual_only  -> return ()
           | otherwise  -> whenWOptM Opt_WarnMissingImportList $ do
                             let msg = TcRnUnknownMessage $
                                   mkPlainDiagnostic (WarningWithFlag Opt_WarnMissingImportList)
                                                     noHints
                                                     (missingImportListWarn imp_mod_name)
                             addDiagnostic msg


    iface <- loadSrcInterface doc imp_mod_name want_boot pkg_qual

    -- Compiler sanity check: if the import didn't say
    -- {-# SOURCE #-} we should not get a hi-boot file
    warnPprTrace ((want_boot == NotBoot) && (mi_boot iface == IsBoot)) (ppr imp_mod_name) $ do

    -- Issue a user warning for a redundant {- SOURCE -} import
    -- NB that we arrange to read all the ordinary imports before
    -- any of the {- SOURCE -} imports.
    --
    -- in --make and GHCi, the compilation manager checks for this,
    -- and indeed we shouldn't do it here because the existence of
    -- the non-boot module depends on the compilation order, which
    -- is not deterministic.  The hs-boot test can show this up.
    dflags <- getDynFlags
    warnIf ((want_boot == IsBoot) && (mi_boot iface == NotBoot) && isOneShot (ghcMode dflags))
           (warnRedundantSourceImport imp_mod_name)
    when (mod_safe && not (safeImportsOn dflags)) $
        addErr $ TcRnUnknownMessage $ mkPlainError noHints $
          (text "safe import can't be used as Safe Haskell isn't on!"
                $+$ text ("please enable Safe Haskell through either Safe, Trustworthy or Unsafe"))

    let
        qual_mod_name = fmap unLoc as_mod `orElse` imp_mod_name
        imp_spec  = ImpDeclSpec { is_mod = imp_mod_name, is_qual = qual_only,
                                  is_dloc = locA loc, is_as = qual_mod_name }

    -- filter the imports according to the import declaration
    (new_imp_details, gres) <- filterImports iface imp_spec imp_details

    -- for certain error messages, we’d like to know what could be imported
    -- here, if everything were imported
    potential_gres <- mkGlobalRdrEnv . snd <$> filterImports iface imp_spec Nothing

    let gbl_env = mkGlobalRdrEnv gres

        is_hiding | Just (True,_) <- imp_details = True
                  | otherwise                    = False

        -- should the import be safe?
        mod_safe' = mod_safe
                    || (not implicit && safeDirectImpsReq dflags)
                    || (implicit && safeImplicitImpsReq dflags)

    hsc_env <- getTopEnv
    let home_unit = hsc_home_unit hsc_env
        imv = ImportedModsVal
            { imv_name        = qual_mod_name
            , imv_span        = locA loc
            , imv_is_safe     = mod_safe'
            , imv_is_hiding   = is_hiding
            , imv_all_exports = potential_gres
            , imv_qualified   = qual_only
            }
        imports = calculateAvails home_unit iface mod_safe' want_boot (ImportedByUser imv)

    -- Complain if we import a deprecated module
    case mi_warns iface of
       WarnAll txt -> do
         let msg = TcRnUnknownMessage $
               mkPlainDiagnostic (WarningWithFlag Opt_WarnWarningsDeprecations)
                                 noHints
                                 (moduleWarn imp_mod_name txt)
         addDiagnostic msg
       _           -> return ()

    -- Complain about -Wcompat-unqualified-imports violations.
    warnUnqualifiedImport decl iface

    let new_imp_decl = ImportDecl
          { ideclExt       = noExtField
          , ideclSourceSrc = ideclSourceSrc decl
          , ideclName      = ideclName decl
          , ideclPkgQual   = pkg_qual
          , ideclSource    = ideclSource decl
          , ideclSafe      = mod_safe'
          , ideclQualified = ideclQualified decl
          , ideclImplicit  = ideclImplicit decl
          , ideclAs        = ideclAs decl
          , ideclHiding    = new_imp_details
          }

    return (L loc new_imp_decl, gbl_env, imports, mi_hpc iface)


-- | Rename raw package imports
renameRawPkgQual :: UnitEnv -> RawPkgQual -> PkgQual
renameRawPkgQual unit_env = \case
  NoRawPkgQual -> NoPkgQual
  RawPkgQual p -> renamePkgQual unit_env (Just (sl_fs p))

-- | Rename raw package imports
renamePkgQual :: UnitEnv -> Maybe FastString -> PkgQual
renamePkgQual unit_env mb_pkg = case mb_pkg of
  Nothing -> NoPkgQual
  Just pkg_fs
    | Just uid <- homeUnitId <$> ue_home_unit unit_env
    , pkg_fs == fsLit "this" || pkg_fs == unitFS uid
    -> ThisPkg uid

    | Just uid <- lookupPackageName (ue_units unit_env) (PackageName pkg_fs)
    -> OtherPkg uid

    | otherwise
    -> OtherPkg (UnitId pkg_fs)
       -- not really correct as pkg_fs is unlikely to be a valid unit-id but
       -- we will report the failure later...

-- | Calculate the 'ImportAvails' induced by an import of a particular
-- interface, but without 'imp_mods'.
calculateAvails :: HomeUnit
                -> ModIface
                -> IsSafeImport
                -> IsBootInterface
                -> ImportedBy
                -> ImportAvails
calculateAvails home_unit iface mod_safe' want_boot imported_by =
  let imp_mod    = mi_module iface
      imp_sem_mod= mi_semantic_module iface
      orph_iface = mi_orphan (mi_final_exts iface)
      has_finsts = mi_finsts (mi_final_exts iface)
      deps       = mi_deps iface
      trust      = getSafeMode $ mi_trust iface
      trust_pkg  = mi_trust_pkg iface
      is_sig     = mi_hsc_src iface == HsigFile

      -- If the module exports anything defined in this module, just
      -- ignore it.  Reason: otherwise it looks as if there are two
      -- local definition sites for the thing, and an error gets
      -- reported.  Easiest thing is just to filter them out up
      -- front. This situation only arises if a module imports
      -- itself, or another module that imported it.  (Necessarily,
      -- this involves a loop.)
      --
      -- We do this *after* filterImports, so that if you say
      --      module A where
      --         import B( AType )
      --         type AType = ...
      --
      --      module B( AType ) where
      --         import {-# SOURCE #-} A( AType )
      --
      -- then you won't get a 'B does not export AType' message.


      -- Compute new transitive dependencies
      --
      -- 'dep_orphs' and 'dep_finsts' do NOT include the imported module
      -- itself, but we DO need to include this module in 'imp_orphs' and
      -- 'imp_finsts' if it defines an orphan or instance family; thus the
      -- orph_iface/has_iface tests.

      deporphs  = dep_orphs deps
      depfinsts = dep_finsts deps

      orphans | orph_iface = assertPpr (not (imp_sem_mod `elem` deporphs)) (ppr imp_sem_mod <+> ppr deporphs) $
                             imp_sem_mod : deporphs
              | otherwise  = deporphs

      finsts | has_finsts = assertPpr (not (imp_sem_mod `elem` depfinsts)) (ppr imp_sem_mod <+> ppr depfinsts) $
                            imp_sem_mod : depfinsts
             | otherwise  = depfinsts

      -- Trusted packages are a lot like orphans.
      trusted_pkgs | mod_safe' = dep_trusted_pkgs deps
                   | otherwise = S.empty


      pkg = moduleUnit (mi_module iface)
      ipkg = toUnitId pkg

      -- Does this import mean we now require our own pkg
      -- to be trusted? See Note [Trust Own Package]
      ptrust = trust == Sf_Trustworthy || trust_pkg
      pkg_trust_req
        | isHomeUnit home_unit pkg = ptrust
        | otherwise = False

      dependent_pkgs = if isHomeUnit home_unit pkg
                        then S.empty
                        else S.singleton ipkg

      direct_mods = mkModDeps $ if isHomeUnit home_unit pkg
                      then S.singleton (GWIB (moduleName imp_mod) want_boot)
                      else S.empty

      dep_boot_mods_map = mkModDeps (dep_boot_mods deps)

      boot_mods
        -- If we are looking for a boot module, it must be HPT
        | IsBoot <- want_boot = addToUFM dep_boot_mods_map (moduleName imp_mod) (GWIB (moduleName imp_mod) IsBoot)
        -- Now we are importing A properly, so don't go looking for
        -- A.hs-boot
        | isHomeUnit home_unit pkg = dep_boot_mods_map
        -- There's no boot files to find in external imports
        | otherwise = emptyUFM

      sig_mods =
        if is_sig
          then moduleName imp_mod : dep_sig_mods deps
          else dep_sig_mods deps


  in ImportAvails {
          imp_mods       = unitModuleEnv (mi_module iface) [imported_by],
          imp_orphs      = orphans,
          imp_finsts     = finsts,
          imp_sig_mods   = sig_mods,
          imp_direct_dep_mods = direct_mods,
          imp_dep_direct_pkgs = dependent_pkgs,
          imp_boot_mods = boot_mods,

          -- Add in the imported modules trusted package
          -- requirements. ONLY do this though if we import the
          -- module as a safe import.
          -- See Note [Tracking Trust Transitively]
          -- and Note [Trust Transitive Property]
          imp_trust_pkgs = trusted_pkgs,
          -- Do we require our own pkg to be trusted?
          -- See Note [Trust Own Package]
          imp_trust_own_pkg = pkg_trust_req
     }


-- | Issue a warning if the user imports Data.List without either an import
-- list or `qualified`. This is part of the migration plan for the
-- `Data.List.singleton` proposal. See #17244.
--
-- Currently not used for anything.
warnUnqualifiedImport :: ImportDecl GhcPs -> ModIface -> RnM ()
warnUnqualifiedImport decl iface =
    when bad_import $ do
      let msg = TcRnUnknownMessage $
            mkPlainDiagnostic (WarningWithFlag Opt_WarnCompatUnqualifiedImports)
                              noHints
                              warning
      addDiagnosticAt loc msg
  where
    mod = mi_module iface
    loc = getLocA $ ideclName decl

    is_qual = isImportDeclQualified (ideclQualified decl)
    has_import_list =
      -- We treat a `hiding` clause as not having an import list although
      -- it's not entirely clear this is the right choice.
      case ideclHiding decl of
        Just (False, _) -> True
        _               -> False
    bad_import =
         not is_qual
      && not has_import_list
      && mod `elemModuleSet` qualifiedMods

    warning = vcat
      [ text "To ensure compatibility with future core libraries changes"
      , text "imports to" <+> ppr (ideclName decl) <+> text "should be"
      , text "either qualified or have an explicit import list."
      ]

    -- Modules for which we warn if we see unqualified imports
    -- Currently empty.
    qualifiedMods = mkModuleSet []


warnRedundantSourceImport :: ModuleName -> TcRnMessage
warnRedundantSourceImport mod_name
  = TcRnUnknownMessage $ mkPlainDiagnostic WarningWithoutFlag noHints $
      text "Unnecessary {-# SOURCE #-} in the import of module" <+> quotes (ppr mod_name)

{-
************************************************************************
*                                                                      *
\subsection{importsFromLocalDecls}
*                                                                      *
************************************************************************

From the top-level declarations of this module produce
        * the lexical environment
        * the ImportAvails
created by its bindings.

Note [Top-level Names in Template Haskell decl quotes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also: Note [Interactively-bound Ids in GHCi] in GHC.Driver.Env
          Note [Looking up Exact RdrNames] in GHC.Rename.Env

Consider a Template Haskell declaration quotation like this:
      module M where
        f x = h [d| f = 3 |]
When renaming the declarations inside [d| ...|], we treat the
top level binders specially in two ways

1.  We give them an Internal Name, not (as usual) an External one.
    This is done by GHC.Rename.Env.newTopSrcBinder.

2.  We make them *shadow* the outer bindings.
    See Note [GlobalRdrEnv shadowing]

3. We find out whether we are inside a [d| ... |] by testing the TH
   stage. This is a slight hack, because the stage field was really
   meant for the type checker, and here we are not interested in the
   fields of Brack, hence the error thunks in thRnBrack.
-}

extendGlobalRdrEnvRn :: [AvailInfo]
                     -> MiniFixityEnv
                     -> RnM (TcGblEnv, TcLclEnv)
-- Updates both the GlobalRdrEnv and the FixityEnv
-- We return a new TcLclEnv only because we might have to
-- delete some bindings from it;
-- see Note [Top-level Names in Template Haskell decl quotes]

extendGlobalRdrEnvRn avails new_fixities
  = do  { (gbl_env, lcl_env) <- getEnvs
        ; stage <- getStage
        ; isGHCi <- getIsGHCi
        ; let rdr_env  = tcg_rdr_env gbl_env
              fix_env  = tcg_fix_env gbl_env
              th_bndrs = tcl_th_bndrs lcl_env
              th_lvl   = thLevel stage

              -- Delete new_occs from global and local envs
              -- If we are in a TemplateHaskell decl bracket,
              --    we are going to shadow them
              -- See Note [GlobalRdrEnv shadowing]
              inBracket = isBrackStage stage

              lcl_env_TH = lcl_env { tcl_rdr = minusLocalRdrEnv (tcl_rdr lcl_env) new_occs }
                           -- See Note [GlobalRdrEnv shadowing]

              lcl_env2 | inBracket = lcl_env_TH
                       | otherwise = lcl_env

              -- Deal with shadowing: see Note [GlobalRdrEnv shadowing]
              want_shadowing = isGHCi || inBracket
              rdr_env1 | want_shadowing = shadowNames rdr_env new_occs
                       | otherwise      = rdr_env

              lcl_env3 = lcl_env2 { tcl_th_bndrs = extendNameEnvList th_bndrs
                                                       [ ( greNameMangledName n
                                                         , (TopLevel, th_lvl) )
                                                       | n <- new_names ] }

        ; rdr_env2 <- foldlM add_gre rdr_env1 new_gres

        ; let fix_env' = foldl' extend_fix_env fix_env new_gres
              gbl_env' = gbl_env { tcg_rdr_env = rdr_env2, tcg_fix_env = fix_env' }

        ; traceRn "extendGlobalRdrEnvRn 2" (pprGlobalRdrEnv True rdr_env2)
        ; return (gbl_env', lcl_env3) }
  where
    new_names = concatMap availGreNames avails
    new_occs  = occSetToEnv (mkOccSet (map occName new_names))

    -- If there is a fixity decl for the gre, add it to the fixity env
    extend_fix_env fix_env gre
      | Just (L _ fi) <- lookupFsEnv new_fixities (occNameFS occ)
      = extendNameEnv fix_env name (FixItem occ fi)
      | otherwise
      = fix_env
      where
        name = greMangledName gre
        occ  = greOccName gre

    new_gres :: [GlobalRdrElt]  -- New LocalDef GREs, derived from avails
    new_gres = concatMap localGREsFromAvail avails

    add_gre :: GlobalRdrEnv -> GlobalRdrElt -> RnM GlobalRdrEnv
    -- Extend the GlobalRdrEnv with a LocalDef GRE
    -- If there is already a LocalDef GRE with the same OccName,
    --    report an error and discard the new GRE
    -- This establishes INVARIANT 1 of GlobalRdrEnvs
    add_gre env gre
      | not (null dups)    -- Same OccName defined twice
      = do { addDupDeclErr (gre : dups); return env }

      | otherwise
      = return (extendGlobalRdrEnv env gre)
      where
        -- See Note [Reporting duplicate local declarations]
        dups = filter isDupGRE (lookupGlobalRdrEnv env (greOccName gre))
        isDupGRE gre' = isLocalGRE gre' && not (isAllowedDup gre')
        isAllowedDup gre' =
            case (isRecFldGRE gre, isRecFldGRE gre') of
              (True,  True)  -> gre_name gre /= gre_name gre'
                                  && isDuplicateRecFldGRE gre'
              (True,  False) -> isNoFieldSelectorGRE gre
              (False, True)  -> isNoFieldSelectorGRE gre'
              (False, False) -> False

{-
Note [Reporting duplicate local declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general, a single module may not define the same OccName multiple times. This
is checked in extendGlobalRdrEnvRn: when adding a new locally-defined GRE to the
GlobalRdrEnv we report an error if there are already duplicates in the
environment.  This establishes INVARIANT 1 (see comments on GlobalRdrEnv in
GHC.Types.Name.Reader), which says that for a given OccName, all the
GlobalRdrElts to which it maps must have distinct 'gre_name's.

For example, the following will be rejected:

  f x = x
  g x = x
  f x = x  -- Duplicate!

Two GREs with the same OccName are OK iff:
-------------------------------------------------------------------
  Existing GRE     |          Newly-defined GRE
                   |  NormalGre            FieldGre
-------------------------------------------------------------------
  Imported         |  Always               Always
                   |
  Local NormalGre  |  Never                NoFieldSelectors
                   |
  Local FieldGre   |  NoFieldSelectors     DuplicateRecordFields
                   |                       and not in same record
-------------------------------------------------------------------            -
In this table "NoFieldSelectors" means "NoFieldSelectors was enabled at the
definition site of the fields; ditto "DuplicateRecordFields".  These facts are
recorded in the 'FieldLabel' (but where both GREs are local, both will
necessarily have the same extensions enabled).

More precisely:

* The programmer is allowed to make a new local definition that clashes with an
  imported one (although attempting to refer to either may lead to ambiguity
  errors at use sites).  For example, the following definition is allowed:

    import M (f)
    f x = x

  Thus isDupGRE reports errors only if the existing GRE is a LocalDef.

* When DuplicateRecordFields is enabled, the same field label may be defined in
  multiple records. For example, this is allowed:

    {-# LANGUAGE DuplicateRecordFields #-}
    data S1 = MkS1 { f :: Int }
    data S2 = MkS2 { f :: Int }

  Even though both fields have the same OccName, this does not violate INVARIANT
  1 of the GlobalRdrEnv, because the fields have distinct selector names, which
  form part of the gre_name (see Note [GreNames] in GHC.Types.Name.Reader).

* However, we must be careful to reject the following (#9156):

    {-# LANGUAGE DuplicateRecordFields #-}
    data T = MkT { f :: Int, f :: Int }  -- Duplicate!

  In this case, both 'gre_name's are the same (because the fields belong to the
  same type), and adding them both to the environment would be a violation of
  INVARIANT 1. Thus isAllowedDup checks both GREs have distinct 'gre_name's
  if they are both record fields.

* With DuplicateRecordFields, we reject attempts to define a field and a
  non-field with the same OccName (#17965):

    {-# LANGUAGE DuplicateRecordFields #-}
    f x = x
    data T = MkT { f :: Int}

  In principle this could be supported, but the current "specification" of
  DuplicateRecordFields does not allow it. Thus isAllowedDup checks for
  DuplicateRecordFields only if *both* GREs being compared are record fields.

* However, with NoFieldSelectors, it is possible by design to define a field and
  a non-field with the same OccName:

    {-# LANGUAGE NoFieldSelectors #-}
    f x = x
    data T = MkT { f :: Int}

  Thus isAllowedDup checks for NoFieldSelectors if either the existing or the
  new GRE are record fields.  See Note [NoFieldSelectors] in GHC.Rename.Env.

See also Note [Skipping ambiguity errors at use sites of local declarations] in
GHC.Rename.Utils.
-}


{- *********************************************************************
*                                                                      *
    getLocalDeclBindersd@ returns the names for an HsDecl
             It's used for source code.

        *** See Note [The Naming story] in GHC.Hs.Decls ****
*                                                                      *
********************************************************************* -}

getLocalNonValBinders :: MiniFixityEnv -> HsGroup GhcPs
    -> RnM ((TcGblEnv, TcLclEnv), NameSet)
-- Get all the top-level binders bound the group *except*
-- for value bindings, which are treated separately
-- Specifically we return AvailInfo for
--      * type decls (incl constructors and record selectors)
--      * class decls (including class ops)
--      * associated types
--      * foreign imports
--      * value signatures (in hs-boot files only)

getLocalNonValBinders fixity_env
     (HsGroup { hs_valds  = binds,
                hs_tyclds = tycl_decls,
                hs_fords  = foreign_decls })
  = do  { -- Process all type/class decls *except* family instances
        ; let inst_decls = tycl_decls >>= group_instds
        ; dup_fields_ok <- xopt_DuplicateRecordFields <$> getDynFlags
        ; has_sel <- xopt_FieldSelectors <$> getDynFlags
        ; (tc_avails, tc_fldss)
            <- fmap unzip $ concatMapM (new_tc dup_fields_ok has_sel)
                                 (tyClGroupTyClDecls tycl_decls)
        ; traceRn "getLocalNonValBinders 1" (ppr tc_avails)
        ; envs <- extendGlobalRdrEnvRn tc_avails fixity_env
        ; setEnvs envs $ do {
            -- Bring these things into scope first
            -- See Note [Looking up family names in family instances]

          -- Process all family instances
          -- to bring new data constructors into scope
        ; (nti_availss, nti_fldss) <- mapAndUnzipM (new_assoc dup_fields_ok has_sel)
                                                   inst_decls

          -- Finish off with value binders:
          --    foreign decls and pattern synonyms for an ordinary module
          --    type sigs in case of a hs-boot file only
        ; is_boot <- tcIsHsBootOrSig
        ; let val_bndrs | is_boot   = hs_boot_sig_bndrs
                        | otherwise = for_hs_bndrs
        ; val_avails <- mapM new_simple val_bndrs

        ; let avails    = concat nti_availss ++ val_avails
              new_bndrs = availsToNameSetWithSelectors avails `unionNameSet`
                          availsToNameSetWithSelectors tc_avails
              flds      = concat nti_fldss ++ concat tc_fldss
        ; traceRn "getLocalNonValBinders 2" (ppr avails)
        ; (tcg_env, tcl_env) <- extendGlobalRdrEnvRn avails fixity_env

        -- Force the field access so that tcg_env is not retained. The
        -- selector thunk optimisation doesn't kick-in, see #20139
        ; let !old_field_env = tcg_field_env tcg_env
        -- Extend tcg_field_env with new fields (this used to be the
        -- work of extendRecordFieldEnv)
              field_env = extendNameEnvList old_field_env flds
              envs      = (tcg_env { tcg_field_env = field_env }, tcl_env)

        ; traceRn "getLocalNonValBinders 3" (vcat [ppr flds, ppr field_env])
        ; return (envs, new_bndrs) } }
  where
    ValBinds _ _val_binds val_sigs = binds

    for_hs_bndrs :: [LocatedN RdrName]
    for_hs_bndrs = hsForeignDeclsBinders foreign_decls

    -- In a hs-boot file, the value binders come from the
    --  *signatures*, and there should be no foreign binders
    hs_boot_sig_bndrs = [ L (l2l decl_loc) (unLoc n)
                        | L decl_loc (TypeSig _ ns _) <- val_sigs, n <- ns]

      -- the SrcSpan attached to the input should be the span of the
      -- declaration, not just the name
    new_simple :: LocatedN RdrName -> RnM AvailInfo
    new_simple rdr_name = do{ nm <- newTopSrcBinder rdr_name
                            ; return (avail nm) }

    new_tc :: DuplicateRecordFields -> FieldSelectors -> LTyClDecl GhcPs
           -> RnM [(AvailInfo, [(Name, [FieldLabel])])]
    new_tc dup_fields_ok has_sel tc_decl -- NOT for type/data instances
        = do { let (bndrs, flds) = hsLTyClDeclBinders tc_decl
             ; names@(main_name : sub_names) <- mapM (newTopSrcBinder . l2n) bndrs
             ; flds' <- mapM (newRecordSelector dup_fields_ok has_sel sub_names) flds
             ; let fld_env = case unLoc tc_decl of
                     DataDecl { tcdDataDefn = d } -> mk_fld_env d names flds'
                     _                            -> []
             ; partyCtrs <- xoptM LangExt.PartialTypeConstructors
             ; if partyCtrs
               then  case unLoc tc_decl of
                       FamDecl {}
                         -> do { m <- getModule
                               ; wf_name <- newWFGlobalBinder m main_name
                               ; return [(availTC main_name names flds', fld_env)
                                        ,((availTC wf_name [wf_name] []), [])]
                               }
                       ClassDecl {}                 -- pick out each of the sub_names
                                                    -- that look like a tycon and
                                                    -- generate a WF_* name for them
                         -> do { m <- getModule
                               ; let ats_names = filter isTyConName sub_names 
                               ; wf_names <- mapM (newWFGlobalBinder m) ats_names
                               ; -- fmap (\n -> ((availTC n [n] []), [])) wf_names
                               ; return [(availTC main_name (names++wf_names) (flds'), fld_env)]
                                 -- ANI TODO: This is not quite what we want, but a good starter.
                               }
                       _          -> return $ [(availTC main_name names flds', fld_env)]
               else return $ [(availTC main_name names flds', fld_env)]
             }

    newWFGlobalBinder :: Module -> Name -> RnM Name
    newWFGlobalBinder m main_name
          = do { let occ = mkTcOcc $ wF_TC_PREFIX ++ (occNameString . nameOccName $ main_name)
               ; newGlobalBinder m occ (nameSrcSpan main_name) }

    -- Calculate the mapping from constructor names to fields, which
    -- will go in tcg_field_env. It's convenient to do this here where
    -- we are working with a single datatype definition.
    mk_fld_env :: HsDataDefn GhcPs -> [Name] -> [FieldLabel]
               -> [(Name, [FieldLabel])]
    mk_fld_env d names flds = concatMap find_con_flds (dd_cons d)
      where
        find_con_flds (L _ (ConDeclH98 { con_name = L _ rdr
                                       , con_args = RecCon cdflds }))
            = [( find_con_name rdr
               , concatMap find_con_decl_flds (unLoc cdflds) )]
        find_con_flds (L _ (ConDeclGADT { con_names = rdrs
                                        , con_g_args = RecConGADT flds _ }))
            = [ ( find_con_name rdr
                 , concatMap find_con_decl_flds (unLoc flds))
              | L _ rdr <- rdrs ]

        find_con_flds _ = []

        find_con_name rdr
          = expectJust "getLocalNonValBinders/find_con_name" $
              find (\ n -> nameOccName n == rdrNameOcc rdr) names
        find_con_decl_flds (L _ x)
          = map find_con_decl_fld (cd_fld_names x)

        find_con_decl_fld  (L _ (FieldOcc _ (L _ rdr)))
          = expectJust "getLocalNonValBinders/find_con_decl_fld" $
              find (\ fl -> flLabel fl == lbl) flds
          where lbl = occNameFS (rdrNameOcc rdr)

    new_assoc :: DuplicateRecordFields -> FieldSelectors -> LInstDecl GhcPs
              -> RnM ([AvailInfo], [(Name, [FieldLabel])])
    new_assoc _ _ (L _ (TyFamInstD {})) = return ([], [])
      -- type instances don't bind new names

    new_assoc dup_fields_ok has_sel (L _ (DataFamInstD _ d))
      = do { (avail, flds) <- new_di dup_fields_ok has_sel Nothing d
           ; return ([avail], flds) }
    new_assoc dup_fields_ok has_sel (L _ (ClsInstD _ (ClsInstDecl { cid_poly_ty = inst_ty
                                                      , cid_datafam_insts = adts })))
      = do -- First, attempt to grab the name of the class from the instance.
           -- This step could fail if the instance is not headed by a class,
           -- such as in the following examples:
           --
           -- (1) The class is headed by a bang pattern, such as in
           --     `instance !Show Int` (#3811c)
           -- (2) The class is headed by a type variable, such as in
           --     `instance c` (#16385)
           --
           -- If looking up the class name fails, then mb_cls_nm will
           -- be Nothing.
           mb_cls_nm <- runMaybeT $ do
             -- See (1) above
             L loc cls_rdr <- MaybeT $ pure $ getLHsInstDeclClass_maybe inst_ty
             -- See (2) above
             MaybeT $ setSrcSpan (locA loc) $ lookupGlobalOccRn_maybe cls_rdr
           -- Assuming the previous step succeeded, process any associated data
           -- family instances. If the previous step failed, bail out.
           case mb_cls_nm of
             Nothing -> pure ([], [])
             Just cls_nm -> do
               (avails, fldss)
                 <- mapAndUnzipM (new_loc_di dup_fields_ok has_sel (Just cls_nm)) adts
               pure (avails, concat fldss)

    new_di :: DuplicateRecordFields -> FieldSelectors -> Maybe Name -> DataFamInstDecl GhcPs
                   -> RnM (AvailInfo, [(Name, [FieldLabel])])
    new_di dup_fields_ok has_sel mb_cls dfid@(DataFamInstDecl { dfid_eqn = ti_decl })
        = do { main_name <- lookupFamInstName mb_cls (feqn_tycon ti_decl)
             ; let (bndrs, flds) = hsDataFamInstBinders dfid
             ; sub_names <- mapM (newTopSrcBinder .l2n) bndrs
             ; flds' <- mapM (newRecordSelector dup_fields_ok has_sel sub_names) flds
             ; let avail    = availTC (unLoc main_name) sub_names flds'
                                  -- main_name is not bound here!
                   fld_env  = mk_fld_env (feqn_rhs ti_decl) sub_names flds'
             ; return (avail, fld_env) }

    new_loc_di :: DuplicateRecordFields -> FieldSelectors -> Maybe Name -> LDataFamInstDecl GhcPs
                   -> RnM (AvailInfo, [(Name, [FieldLabel])])
    new_loc_di dup_fields_ok has_sel mb_cls (L _ d) = new_di dup_fields_ok has_sel mb_cls d

newRecordSelector :: DuplicateRecordFields -> FieldSelectors -> [Name] -> LFieldOcc GhcPs -> RnM FieldLabel
newRecordSelector _ _ [] _ = error "newRecordSelector: datatype has no constructors!"
newRecordSelector dup_fields_ok has_sel (dc:_) (L loc (FieldOcc _ (L _ fld)))
  = do { selName <- newTopSrcBinder $ L (l2l loc) $ field
       ; return $ FieldLabel { flLabel = fieldLabelString
                             , flHasDuplicateRecordFields = dup_fields_ok
                             , flHasFieldSelector = has_sel
                             , flSelector = selName } }
  where
    fieldLabelString = occNameFS $ rdrNameOcc fld
    selOccName = fieldSelectorOccName fieldLabelString (nameOccName dc) dup_fields_ok has_sel
    field | isExact fld = fld
              -- use an Exact RdrName as is to preserve the bindings
              -- of an already renamer-resolved field and its use
              -- sites. This is needed to correctly support record
              -- selectors in Template Haskell. See Note [Binders in
              -- Template Haskell] in "GHC.ThToHs" and Note [Looking up
              -- Exact RdrNames] in "GHC.Rename.Env".
          | otherwise   = mkRdrUnqual selOccName

{-
Note [Looking up family names in family instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

  module M where
    type family T a :: *
    type instance M.T Int = Bool

We might think that we can simply use 'lookupOccRn' when processing the type
instance to look up 'M.T'.  Alas, we can't!  The type family declaration is in
the *same* HsGroup as the type instance declaration.  Hence, as we are
currently collecting the binders declared in that HsGroup, these binders will
not have been added to the global environment yet.

Solution is simple: process the type family declarations first, extend
the environment, and then process the type instances.


************************************************************************
*                                                                      *
\subsection{Filtering imports}
*                                                                      *
************************************************************************

@filterImports@ takes the @ExportEnv@ telling what the imported module makes
available, and filters it through the import spec (if any).

Note [Dealing with imports]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
For import M( ies ), we take the mi_exports of M, and make
   imp_occ_env :: OccEnv (NameEnv (GreName, AvailInfo, Maybe Name))
One entry for each OccName that M exports, mapping each corresponding Name to
its GreName, the AvailInfo exported from M that exports that Name, and
optionally a Name for an associated type's parent class. (Typically there will
be a single Name in the NameEnv, but see Note [Importing DuplicateRecordFields]
for why we may need more than one.)

The situation is made more complicated by associated types. E.g.
   module M where
     class    C a    where { data T a }
     instance C Int  where { data T Int = T1 | T2 }
     instance C Bool where { data T Int = T3 }
Then M's export_avails are (recall the AvailTC invariant from Avails.hs)
  C(C,T), T(T,T1,T2,T3)
Notice that T appears *twice*, once as a child and once as a parent. From
this list we construct a raw list including
   T -> (T, T( T1, T2, T3 ), Nothing)
   T -> (T, C( C, T ),       Nothing)
and we combine these (in function 'combine' in 'imp_occ_env' in
'filterImports') to get
   T  -> (T,  T(T,T1,T2,T3), Just C)

So the overall imp_occ_env is
   C  -> (C,  C(C,T),        Nothing)
   T  -> (T,  T(T,T1,T2,T3), Just C)
   T1 -> (T1, T(T,T1,T2,T3), Nothing)   -- similarly T2,T3

If we say
   import M( T(T1,T2) )
then we get *two* Avails:  C(T), T(T1,T2)

Note that the imp_occ_env will have entries for data constructors too,
although we never look up data constructors.

Note [Importing PatternSynonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As described in Note [Dealing with imports], associated types can lead to the
same Name appearing twice, both as a child and once as a parent, when
constructing the imp_occ_env.  The same thing can happen with pattern synonyms
if they are exported bundled with a type.

A simplified example, based on #11959:

  {-# LANGUAGE PatternSynonyms #-}
  module M (T(P), pattern P) where  -- Duplicate export warning, but allowed
    data T = MkT
    pattern P = MkT

Here we have T(P) and P in export_avails, and construct both
  P -> (P, P, Nothing)
  P -> (P, T(P), Nothing)
which are 'combine'd to leave
  P -> (P, T(P), Nothing)
i.e. we simply discard the non-bundled Avail.

Note [Importing DuplicateRecordFields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In filterImports, another complicating factor is DuplicateRecordFields.
Suppose we have:

  {-# LANGUAGE DuplicateRecordFields #-}
  module M (S(foo), T(foo)) where
    data S = MkS { foo :: Int }
    data T = mkT { foo :: Int }

  module N where
    import M (foo)    -- this is an ambiguity error (A)
    import M (S(foo)) -- this is allowed (B)

Here M exports the OccName 'foo' twice, so we get an imp_occ_env where 'foo'
maps to a NameEnv containing an entry for each of the two mangled field selector
names (see Note [FieldLabel] in GHC.Types.FieldLabel).

  foo -> [ $sel:foo:MkS -> (foo, S(foo), Nothing)
         , $sel:foo:MKT -> (foo, T(foo), Nothing)
         ]

Then when we look up 'foo' in lookup_name for case (A) we get both entries and
hence report an ambiguity error.  Whereas in case (B) we reach the lookup_ie
case for IEThingWith, which looks up 'S' and then finds the unique 'foo' amongst
its children.

See T16745 for a test of this.

-}

filterImports
    :: ModIface
    -> ImpDeclSpec                     -- The span for the entire import decl
    -> Maybe (Bool, LocatedL [LIE GhcPs])    -- Import spec; True => hiding
    -> RnM (Maybe (Bool, LocatedL [LIE GhcRn]), -- Import spec w/ Names
            [GlobalRdrElt])                   -- Same again, but in GRE form
filterImports iface decl_spec Nothing
  = return (Nothing, gresFromAvails (Just imp_spec) (mi_exports iface))
  where
    imp_spec = ImpSpec { is_decl = decl_spec, is_item = ImpAll }


filterImports iface decl_spec (Just (want_hiding, L l import_items))
  = do  -- check for errors, convert RdrNames to Names
        items1 <- mapM lookup_lie import_items

        let items2 :: [(LIE GhcRn, AvailInfo)]
            items2 = concat items1
                -- NB the AvailInfo may have duplicates, and several items
                --    for the same parent; e.g N(x) and N(y)

            names  = availsToNameSetWithSelectors (map snd items2)
            keep n = not (n `elemNameSet` names)
            pruned_avails = filterAvails keep all_avails
            hiding_spec = ImpSpec { is_decl = decl_spec, is_item = ImpAll }

            gres | want_hiding = gresFromAvails (Just hiding_spec) pruned_avails
                 | otherwise   = concatMap (gresFromIE decl_spec) items2

        return (Just (want_hiding, L l (map fst items2)), gres)
  where
    all_avails = mi_exports iface

        -- See Note [Dealing with imports]
    imp_occ_env :: OccEnv (NameEnv (GreName,    -- the name or field
                           AvailInfo,   -- the export item providing it
                           Maybe Name))   -- the parent of associated types
    imp_occ_env = mkOccEnv_C (plusNameEnv_C combine)
                             [ (occName c, mkNameEnv [(greNameMangledName c, (c, a, Nothing))])
                                     | a <- all_avails
                                     , c <- availGreNames a]
    -- See Note [Dealing with imports]
    -- 'combine' may be called for associated data types which appear
    -- twice in the all_avails. In the example, we combine
    --    T(T,T1,T2,T3) and C(C,T)  to give   (T, T(T,T1,T2,T3), Just C)
    -- NB: the AvailTC can have fields as well as data constructors (#12127)
    combine :: (GreName, AvailInfo, Maybe Name)
            -> (GreName, AvailInfo, Maybe Name)
            -> (GreName, AvailInfo, Maybe Name)
    combine (NormalGreName name1, a1@(AvailTC p1 _), mb1)
            (NormalGreName name2, a2@(AvailTC p2 _), mb2)
      = assertPpr (name1 == name2 && isNothing mb1 && isNothing mb2)
                  (ppr name1 <+> ppr name2 <+> ppr mb1 <+> ppr mb2) $
        if p1 == name1 then (NormalGreName name1, a1, Just p2)
                       else (NormalGreName name1, a2, Just p1)
    -- 'combine' may also be called for pattern synonyms which appear both
    -- unassociated and associated (see Note [Importing PatternSynonyms]).
    combine (c1, a1, mb1) (c2, a2, mb2)
      = assertPpr (c1 == c2 && isNothing mb1 && isNothing mb2
                          && (isAvailTC a1 || isAvailTC a2))
                  (ppr c1 <+> ppr c2 <+> ppr a1 <+> ppr a2 <+> ppr mb1 <+> ppr mb2) $
        if isAvailTC a1 then (c1, a1, Nothing)
                        else (c1, a2, Nothing)

    isAvailTC AvailTC{} = True
    isAvailTC _ = False

    lookup_name :: IE GhcPs -> RdrName -> IELookupM (Name, AvailInfo, Maybe Name)
    lookup_name ie rdr
       | isQual rdr              = failLookupWith (QualImportError rdr)
       | Just succ <- mb_success = case nonDetNameEnvElts succ of
                                     -- See Note [Importing DuplicateRecordFields]
                                     [(c,a,x)] -> return (greNameMangledName c, a, x)
                                     xs -> failLookupWith (AmbiguousImport rdr (map sndOf3 xs))
       | otherwise               = failLookupWith (BadImport ie)
      where
        mb_success = lookupOccEnv imp_occ_env (rdrNameOcc rdr)

    lookup_lie :: LIE GhcPs -> TcRn [(LIE GhcRn, AvailInfo)]
    lookup_lie (L loc ieRdr)
        = do (stuff, warns) <- setSrcSpanA loc $
                               liftM (fromMaybe ([],[])) $
                               run_lookup (lookup_ie ieRdr)
             mapM_ emit_warning warns
             return [ (L loc ie, avail) | (ie,avail) <- stuff ]
        where
            -- Warn when importing T(..) if T was exported abstractly
            emit_warning (DodgyImport n) = whenWOptM Opt_WarnDodgyImports $
              addTcRnDiagnostic (TcRnDodgyImports n)
            emit_warning MissingImportList = whenWOptM Opt_WarnMissingImportList $
              addTcRnDiagnostic (TcRnMissingImportList ieRdr)
            emit_warning (BadImportW ie) = whenWOptM Opt_WarnDodgyImports $ do
              let msg = TcRnUnknownMessage $
                    mkPlainDiagnostic (WarningWithFlag Opt_WarnDodgyImports)
                                      noHints
                                      (lookup_err_msg (BadImport ie))
              addDiagnostic msg

            run_lookup :: IELookupM a -> TcRn (Maybe a)
            run_lookup m = case m of
              Failed err -> do
                addErr $ TcRnUnknownMessage $ mkPlainError noHints (lookup_err_msg err)
                return Nothing
              Succeeded a -> return (Just a)

            lookup_err_msg err = case err of
              BadImport ie  -> badImportItemErr iface decl_spec ie all_avails
              IllegalImport -> illegalImportItemErr
              QualImportError rdr -> qualImportItemErr rdr
              AmbiguousImport rdr xs -> ambiguousImportItemErr rdr xs

        -- For each import item, we convert its RdrNames to Names,
        -- and at the same time construct an AvailInfo corresponding
        -- to what is actually imported by this item.
        -- Returns Nothing on error.
        -- We return a list here, because in the case of an import
        -- item like C, if we are hiding, then C refers to *both* a
        -- type/class and a data constructor.  Moreover, when we import
        -- data constructors of an associated family, we need separate
        -- AvailInfos for the data constructors and the family (as they have
        -- different parents).  See Note [Dealing with imports]
    lookup_ie :: IE GhcPs
              -> IELookupM ([(IE GhcRn, AvailInfo)], [IELookupWarning])
    lookup_ie ie = handle_bad_import $
      case ie of
        IEVar _ (L l n) -> do
            (name, avail, _) <- lookup_name ie $ ieWrappedName n
            return ([(IEVar noExtField (L l (replaceWrappedName n name)),
                                                  trimAvail avail name)], [])

        IEThingAll _ (L l tc) -> do
            (name, avail, mb_parent) <- lookup_name ie $ ieWrappedName tc
            let warns = case avail of
                          Avail {}                     -- e.g. f(..)
                            -> [DodgyImport $ ieWrappedName tc]

                          AvailTC _ subs
                            | null (drop 1 subs) -- e.g. T(..) where T is a synonym
                            -> [DodgyImport $ ieWrappedName tc]

                            | not (is_qual decl_spec)  -- e.g. import M( T(..) )
                            -> [MissingImportList]

                            | otherwise
                            -> []

                renamed_ie = IEThingAll noAnn (L l (replaceWrappedName tc name))
                sub_avails = case avail of
                               Avail {}           -> []
                               AvailTC name2 subs -> [(renamed_ie, AvailTC name2 (subs \\ [NormalGreName name]))]
            case mb_parent of
              Nothing     -> return ([(renamed_ie, avail)], warns)
                             -- non-associated ty/cls
              Just parent -> return ((renamed_ie, AvailTC parent [NormalGreName name]) : sub_avails, warns)
                             -- associated type

        IEThingAbs _ (L l tc')
            | want_hiding   -- hiding ( C )
                       -- Here the 'C' can be a data constructor
                       --  *or* a type/class, or even both
            -> let tc = ieWrappedName tc'
                   tc_name = lookup_name ie tc
                   dc_name = lookup_name ie (setRdrNameSpace tc srcDataName)
               in
               case catIELookupM [ tc_name, dc_name ] of
                 []    -> failLookupWith (BadImport ie)
                 names -> return ([mkIEThingAbs tc' l name | name <- names], [])
            | otherwise
            -> do nameAvail <- lookup_name ie (ieWrappedName tc')
                  return ([mkIEThingAbs tc' l nameAvail]
                         , [])

        IEThingWith xt ltc@(L l rdr_tc) wc rdr_ns -> do
           (name, avail, mb_parent)
               <- lookup_name (IEThingAbs noAnn ltc) (ieWrappedName rdr_tc)

           -- Look up the children in the sub-names of the parent
           -- See Note [Importing DuplicateRecordFields]
           let subnames = availSubordinateGreNames avail
           case lookupChildren subnames rdr_ns of

             Failed rdrs -> failLookupWith (BadImport (IEThingWith xt ltc wc rdrs))
                                -- We are trying to import T( a,b,c,d ), and failed
                                -- to find 'b' and 'd'.  So we make up an import item
                                -- to report as failing, namely T( b, d ).
                                -- c.f. #15412

             Succeeded (childnames, childflds) ->
               case mb_parent of
                 -- non-associated ty/cls
                 Nothing
                   -> return ([(IEThingWith childflds (L l name') wc childnames',
                               availTC name (name:map unLoc childnames) (map unLoc childflds))],
                              [])
                   where name' = replaceWrappedName rdr_tc name
                         childnames' = map to_ie_post_rn childnames
                         -- childnames' = postrn_ies childnames
                 -- associated ty
                 Just parent
                   -> return ([(IEThingWith childflds (L l name') wc childnames',
                                availTC name (map unLoc childnames) (map unLoc childflds)),
                               (IEThingWith childflds (L l name') wc childnames',
                                availTC parent [name] [])],
                              [])
                   where name' = replaceWrappedName rdr_tc name
                         childnames' = map to_ie_post_rn childnames

        _other -> failLookupWith IllegalImport
        -- could be IEModuleContents, IEGroup, IEDoc, IEDocNamed
        -- all errors.

      where
        mkIEThingAbs tc l (n, av, Nothing    )
          = (IEThingAbs noAnn (L l (replaceWrappedName tc n)), trimAvail av n)
        mkIEThingAbs tc l (n, _,  Just parent)
          = (IEThingAbs noAnn (L l (replaceWrappedName tc n))
             , availTC parent [n] [])

        handle_bad_import m = catchIELookup m $ \err -> case err of
          BadImport ie | want_hiding -> return ([], [BadImportW ie])
          _                          -> failLookupWith err

type IELookupM = MaybeErr IELookupError

data IELookupWarning
  = BadImportW (IE GhcPs)
  | MissingImportList
  | DodgyImport RdrName
  -- NB. use the RdrName for reporting a "dodgy" import

data IELookupError
  = QualImportError RdrName
  | BadImport (IE GhcPs)
  | IllegalImport
  | AmbiguousImport RdrName [AvailInfo] -- e.g. a duplicated field name as a top-level import

failLookupWith :: IELookupError -> IELookupM a
failLookupWith err = Failed err

catchIELookup :: IELookupM a -> (IELookupError -> IELookupM a) -> IELookupM a
catchIELookup m h = case m of
  Succeeded r -> return r
  Failed err  -> h err

catIELookupM :: [IELookupM a] -> [a]
catIELookupM ms = [ a | Succeeded a <- ms ]

{-
************************************************************************
*                                                                      *
\subsection{Import/Export Utils}
*                                                                      *
************************************************************************
-}

-- | Given an import\/export spec, construct the appropriate 'GlobalRdrElt's.
gresFromIE :: ImpDeclSpec -> (LIE GhcRn, AvailInfo) -> [GlobalRdrElt]
gresFromIE decl_spec (L loc ie, avail)
  = gresFromAvail prov_fn avail
  where
    is_explicit = case ie of
                    IEThingAll _ name -> \n -> n == lieWrappedName name
                    _                 -> \_ -> True
    prov_fn name
      = Just (ImpSpec { is_decl = decl_spec, is_item = item_spec })
      where
        item_spec = ImpSome { is_explicit = is_explicit name
                            , is_iloc = locA loc }


{-
Note [Children for duplicate record fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the module

    {-# LANGUAGE DuplicateRecordFields #-}
    module M (F(foo, MkFInt, MkFBool)) where
      data family F a
      data instance F Int = MkFInt { foo :: Int }
      data instance F Bool = MkFBool { foo :: Bool }

The `foo` in the export list refers to *both* selectors! For this
reason, lookupChildren builds an environment that maps the FastString
to a list of items, rather than a single item.
-}

mkChildEnv :: [GlobalRdrElt] -> NameEnv [GlobalRdrElt]
mkChildEnv gres = foldr add emptyNameEnv gres
  where
    add gre env = case gre_par gre of
        ParentIs  p -> extendNameEnv_Acc (:) Utils.singleton env p gre
        NoParent    -> env

findChildren :: NameEnv [a] -> Name -> [a]
findChildren env n = lookupNameEnv env n `orElse` []

lookupChildren :: [GreName] -> [LIEWrappedName RdrName]
               -> MaybeErr [LIEWrappedName RdrName]   -- The ones for which the lookup failed
                           ([LocatedA Name], [Located FieldLabel])
-- (lookupChildren all_kids rdr_items) maps each rdr_item to its
-- corresponding Name all_kids, if the former exists
-- The matching is done by FastString, not OccName, so that
--    Cls( meth, AssocTy )
-- will correctly find AssocTy among the all_kids of Cls, even though
-- the RdrName for AssocTy may have a (bogus) DataName namespace
-- (Really the rdr_items should be FastStrings in the first place.)
lookupChildren all_kids rdr_items
  | null fails
  = Succeeded (fmap concat (partitionEithers oks))
       -- This 'fmap concat' trickily applies concat to the /second/ component
       -- of the pair, whose type is ([LocatedA Name], [[Located FieldLabel]])
  | otherwise
  = Failed fails
  where
    mb_xs = map doOne rdr_items
    fails = [ bad_rdr | Failed bad_rdr <- mb_xs ]
    oks   = [ ok      | Succeeded ok   <- mb_xs ]
    oks :: [Either (LocatedA Name) [Located FieldLabel]]

    doOne item@(L l r)
       = case (lookupFsEnv kid_env . occNameFS . rdrNameOcc . ieWrappedName) r of
           Just [NormalGreName n]                             -> Succeeded (Left (L l n))
           Just rs | Just fs <- traverse greNameFieldLabel rs -> Succeeded (Right (map (L (locA l)) fs))
           _                                                  -> Failed    item

    -- See Note [Children for duplicate record fields]
    kid_env = extendFsEnvList_C (++) emptyFsEnv
              [(occNameFS (occName x), [x]) | x <- all_kids]



-------------------------------

{-
*********************************************************
*                                                       *
\subsection{Unused names}
*                                                       *
*********************************************************
-}

reportUnusedNames :: TcGblEnv -> HscSource -> RnM ()
reportUnusedNames gbl_env hsc_src
  = do  { keep <- readTcRef (tcg_keep gbl_env)
        ; traceRn "RUN" (ppr (tcg_dus gbl_env))
        ; warnUnusedImportDecls gbl_env hsc_src
        ; warnUnusedTopBinds $ unused_locals keep
        ; warnMissingSignatures gbl_env
        ; warnMissingKindSignatures gbl_env }
  where
    used_names :: NameSet -> NameSet
    used_names keep = findUses (tcg_dus gbl_env) emptyNameSet `unionNameSet` keep
    -- NB: currently, if f x = g, we only treat 'g' as used if 'f' is used
    -- Hence findUses

    -- Collect the defined names from the in-scope environment
    defined_names :: [GlobalRdrElt]
    defined_names = globalRdrEnvElts (tcg_rdr_env gbl_env)

    kids_env = mkChildEnv defined_names
    -- This is done in mkExports too; duplicated work

    gre_is_used :: NameSet -> GlobalRdrElt -> Bool
    gre_is_used used_names gre0
        = name `elemNameSet` used_names
          || any (\ gre -> greMangledName gre `elemNameSet` used_names) (findChildren kids_env name)
                -- A use of C implies a use of T,
                -- if C was brought into scope by T(..) or T(C)
      where
        name = greMangledName gre0

    -- Filter out the ones that are
    --  (a) defined in this module, and
    --  (b) not defined by a 'deriving' clause
    -- The latter have an Internal Name, so we can filter them out easily
    unused_locals :: NameSet -> [GlobalRdrElt]
    unused_locals keep =
      let -- Note that defined_and_used, defined_but_not_used
          -- are both [GRE]; that's why we need defined_and_used
          -- rather than just used_names
          _defined_and_used, defined_but_not_used :: [GlobalRdrElt]
          (_defined_and_used, defined_but_not_used)
              = partition (gre_is_used (used_names keep)) defined_names

      in filter is_unused_local defined_but_not_used
    is_unused_local :: GlobalRdrElt -> Bool
    is_unused_local gre = isLocalGRE gre && isExternalName (greMangledName gre)

{- *********************************************************************
*                                                                      *
              Missing signatures
*                                                                      *
********************************************************************* -}

{-
Note [Missing signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~
There are four warning flags in play:

  * -Wmissing-exported-signatures
    Warn about any exported top-level function/value without a type signature.
    Does not include pattern synonyms.

  * -Wmissing-signatures
    Warn about any top-level function/value without a type signature. Does not
    include pattern synonyms. Takes priority over -Wmissing-exported-signatures.

  * -Wmissing-exported-pattern-synonym-signatures
    Warn about any exported pattern synonym without a type signature.

  * -Wmissing-pattern-synonym-signatures
    Warn about any pattern synonym without a type signature. Takes priority over
    -Wmissing-exported-pattern-synonym-signatures.

-}

-- | Warn the user about top level binders that lack type signatures.
-- Called /after/ type inference, so that we can report the
-- inferred type of the function
warnMissingSignatures :: TcGblEnv -> RnM ()
warnMissingSignatures gbl_env
  = do { let exports = availsToNameSet (tcg_exports gbl_env)
             sig_ns  = tcg_sigs gbl_env
               -- We use sig_ns to exclude top-level bindings that are generated by GHC
             binds    = collectHsBindsBinders CollNoDictBinders $ tcg_binds gbl_env
             pat_syns = tcg_patsyns gbl_env

         -- Warn about missing signatures
         -- Do this only when we have a type to offer
       ; warn_binds             <- woptM Opt_WarnMissingSignatures
       ; warn_exported_binds    <- woptM Opt_WarnMissingExportedSignatures
       ; warn_pat_syns          <- woptM Opt_WarnMissingPatternSynonymSignatures
       ; warn_exported_pat_syns <- woptM Opt_WarnMissingExportedPatternSynonymSignatures

         -- See Note [Missing signatures]
       ; let add_sig_warns
               = when (warn_pat_syns || warn_exported_pat_syns)
                      (mapM_ add_pat_syn_warn pat_syns) >>
                 when (warn_binds || warn_exported_binds)
                      (mapM_ add_bind_warn binds)

             add_pat_syn_warn p
               = when export_check $
                 add_warn name flag $
                 hang (text "Pattern synonym with no type signature:")
                    2 (text "pattern" <+> pprPrefixName name <+> dcolon <+> pp_ty)
               where
                 name  = patSynName p
                 pp_ty = pprPatSynType p
                 export_check = warn_pat_syns || name `elemNameSet` exports
                 flag | warn_pat_syns
                      = Opt_WarnMissingPatternSynonymSignatures
                      | otherwise
                      = Opt_WarnMissingExportedPatternSynonymSignatures

             add_bind_warn :: Id -> IOEnv (Env TcGblEnv TcLclEnv) ()
             add_bind_warn id
               = do { env <- tcInitTidyEnv     -- Why not use emptyTidyEnv?
                    ; let (_, ty) = tidyOpenType env (idType id)
                          ty_msg  = pprSigmaType ty

                    ; when export_check $
                      add_warn name flag $
                      hang (text "Top-level binding with no type signature:")
                         2 (pprPrefixName name <+> dcolon <+> ty_msg) }
               where
                 name = idName id
                 export_check = warn_binds || name `elemNameSet` exports
                 flag | warn_binds
                      = Opt_WarnMissingSignatures
                      | otherwise
                      = Opt_WarnMissingExportedSignatures

             add_warn name flag msg
               = when not_ghc_generated $ do
                   let dia = TcRnUnknownMessage $
                         mkPlainDiagnostic (WarningWithFlag flag) noHints msg
                   addDiagnosticAt (getSrcSpan name) dia
               where
                 not_ghc_generated
                   = name `elemNameSet` sig_ns

       ; add_sig_warns }

-- | Warn the user about tycons that lack kind signatures.
-- Called /after/ type (and kind) inference, so that we can report the
-- inferred kinds.
warnMissingKindSignatures :: TcGblEnv -> RnM ()
warnMissingKindSignatures gbl_env
  = do { warn_missing_kind_sigs  <- woptM Opt_WarnMissingKindSignatures
       ; cusks_enabled <- xoptM LangExt.CUSKs
       ; when (warn_missing_kind_sigs) (mapM_ (add_ty_warn cusks_enabled) tcs)
       }
  where
    tcs = tcg_tcs gbl_env
    ksig_ns = tcg_ksigs gbl_env

    add_ty_warn :: Bool -> TyCon -> IOEnv (Env TcGblEnv TcLclEnv) ()
    add_ty_warn cusks_enabled tyCon = when (name `elemNameSet` ksig_ns) $ do
        let dia = TcRnUnknownMessage $
              mkPlainDiagnostic (WarningWithFlag Opt_WarnMissingKindSignatures) noHints $
                hang msg 2 (text "type" <+> pprPrefixName name <+> dcolon <+> ki_msg)
        addDiagnosticAt (getSrcSpan name) dia
      where
        msg | cusks_enabled = text "Top-level type constructor with no standalone kind signature or CUSK:"
            | otherwise     = text "Top-level type constructor with no standalone kind signature:"
        name = tyConName tyCon
        ki = tyConKind tyCon
        ki_msg :: SDoc
        ki_msg = pprKind ki

{-
*********************************************************
*                                                       *
\subsection{Unused imports}
*                                                       *
*********************************************************

This code finds which import declarations are unused.  The
specification and implementation notes are here:
  https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/unused-imports

See also Note [Choosing the best import declaration] in GHC.Types.Name.Reader
-}

type ImportDeclUsage
   = ( LImportDecl GhcRn   -- The import declaration
     , [GlobalRdrElt]      -- What *is* used (normalised)
     , [Name] )            -- What is imported but *not* used

warnUnusedImportDecls :: TcGblEnv -> HscSource -> RnM ()
warnUnusedImportDecls gbl_env hsc_src
  = do { uses <- readMutVar (tcg_used_gres gbl_env)
       ; let user_imports = filterOut
                              (ideclImplicit . unLoc)
                              (tcg_rn_imports gbl_env)
                -- This whole function deals only with *user* imports
                -- both for warning about unnecessary ones, and for
                -- deciding the minimal ones
             rdr_env = tcg_rdr_env gbl_env
             fld_env = mkFieldEnv rdr_env

       ; let usage :: [ImportDeclUsage]
             usage = findImportUsage user_imports uses

       ; traceRn "warnUnusedImportDecls" $
                       (vcat [ text "Uses:" <+> ppr uses
                             , text "Import usage" <+> ppr usage])

       ; whenWOptM Opt_WarnUnusedImports $
         mapM_ (warnUnusedImport Opt_WarnUnusedImports fld_env) usage

       ; whenGOptM Opt_D_dump_minimal_imports $
         printMinimalImports hsc_src usage }

findImportUsage :: [LImportDecl GhcRn]
                -> [GlobalRdrElt]
                -> [ImportDeclUsage]

findImportUsage imports used_gres
  = map unused_decl imports
  where
    import_usage :: ImportMap
    import_usage = mkImportMap used_gres

    unused_decl :: LImportDecl GhcRn -> (LImportDecl GhcRn, [GlobalRdrElt], [Name])
    unused_decl decl@(L loc (ImportDecl { ideclHiding = imps }))
      = (decl, used_gres, nameSetElemsStable unused_imps)
      where
        used_gres = lookupSrcLoc (srcSpanEnd $ locA loc) import_usage
                               -- srcSpanEnd: see Note [The ImportMap]
                    `orElse` []

        used_names   = mkNameSet (map      greMangledName        used_gres)
        used_parents = mkNameSet (mapMaybe greParent_maybe used_gres)

        unused_imps   -- Not trivial; see eg #7454
          = case imps of
              Just (False, L _ imp_ies) ->
                                 foldr (add_unused . unLoc) emptyNameSet imp_ies
              _other -> emptyNameSet -- No explicit import list => no unused-name list

        add_unused :: IE GhcRn -> NameSet -> NameSet
        add_unused (IEVar _ n)      acc = add_unused_name (lieWrappedName n) acc
        add_unused (IEThingAbs _ n) acc = add_unused_name (lieWrappedName n) acc
        add_unused (IEThingAll _ n) acc = add_unused_all  (lieWrappedName n) acc
        add_unused (IEThingWith fs p wc ns) acc =
          add_wc_all (add_unused_with pn xs acc)
          where pn = lieWrappedName p
                xs = map lieWrappedName ns ++ map (flSelector . unLoc) fs
                add_wc_all = case wc of
                            NoIEWildcard -> id
                            IEWildcard _ -> add_unused_all pn
        add_unused _ acc = acc

        add_unused_name n acc
          | n `elemNameSet` used_names = acc
          | otherwise                  = acc `extendNameSet` n
        add_unused_all n acc
          | n `elemNameSet` used_names   = acc
          | n `elemNameSet` used_parents = acc
          | otherwise                    = acc `extendNameSet` n
        add_unused_with p ns acc
          | all (`elemNameSet` acc1) ns = add_unused_name p acc1
          | otherwise = acc1
          where
            acc1 = foldr add_unused_name acc ns
       -- If you use 'signum' from Num, then the user may well have
       -- imported Num(signum).  We don't want to complain that
       -- Num is not itself mentioned.  Hence the two cases in add_unused_with.


{- Note [The ImportMap]
~~~~~~~~~~~~~~~~~~~~~~~
The ImportMap is a short-lived intermediate data structure records, for
each import declaration, what stuff brought into scope by that
declaration is actually used in the module.

The SrcLoc is the location of the END of a particular 'import'
declaration.  Why *END*?  Because we don't want to get confused
by the implicit Prelude import. Consider (#7476) the module
    import Foo( foo )
    main = print foo
There is an implicit 'import Prelude(print)', and it gets a SrcSpan
of line 1:1 (just the point, not a span). If we use the *START* of
the SrcSpan to identify the import decl, we'll confuse the implicit
import Prelude with the explicit 'import Foo'.  So we use the END.
It's just a cheap hack; we could equally well use the Span too.

The [GlobalRdrElt] are the things imported from that decl.
-}

type ImportMap = Map RealSrcLoc [GlobalRdrElt]  -- See [The ImportMap]
     -- If loc :-> gres, then
     --   'loc' = the end loc of the bestImport of each GRE in 'gres'

mkImportMap :: [GlobalRdrElt] -> ImportMap
-- For each of a list of used GREs, find all the import decls that brought
-- it into scope; choose one of them (bestImport), and record
-- the RdrName in that import decl's entry in the ImportMap
mkImportMap gres
  = foldr add_one Map.empty gres
  where
    add_one gre@(GRE { gre_imp = imp_specs }) imp_map =
      case srcSpanEnd (is_dloc (is_decl best_imp_spec)) of
                              -- For srcSpanEnd see Note [The ImportMap]
       RealSrcLoc decl_loc _ -> Map.insertWith add decl_loc [gre] imp_map
       UnhelpfulLoc _ -> imp_map
       where
          best_imp_spec = bestImport (bagToList imp_specs)
          add _ gres = gre : gres

warnUnusedImport :: WarningFlag -> NameEnv (FieldLabelString, Parent)
                 -> ImportDeclUsage -> RnM ()
warnUnusedImport flag fld_env (L loc decl, used, unused)

  -- Do not warn for 'import M()'
  | Just (False,L _ []) <- ideclHiding decl
  = return ()

  -- Note [Do not warn about Prelude hiding]
  | Just (True, L _ hides) <- ideclHiding decl
  , not (null hides)
  , pRELUDE_NAME == unLoc (ideclName decl)
  = return ()

  -- Nothing used; drop entire declaration
  | null used
  = let dia = TcRnUnknownMessage $
          mkPlainDiagnostic (WarningWithFlag flag) noHints msg1
    in addDiagnosticAt (locA loc) dia

  -- Everything imported is used; nop
  | null unused
  = return ()

  -- Only one import is unused, with `SrcSpan` covering only the unused item instead of
  -- the whole import statement
  | Just (_, L _ imports) <- ideclHiding decl
  , length unused == 1
  , Just (L loc _) <- find (\(L _ ie) -> ((ieName ie) :: Name) `elem` unused) imports
  = let dia = TcRnUnknownMessage $ mkPlainDiagnostic (WarningWithFlag flag) noHints msg2
    in addDiagnosticAt (locA loc) dia

  -- Some imports are unused
  | otherwise
  = let dia = TcRnUnknownMessage $ mkPlainDiagnostic (WarningWithFlag flag) noHints msg2
    in addDiagnosticAt (locA loc) dia

  where
    msg1 = vcat [ pp_herald <+> quotes pp_mod <+> is_redundant
                , nest 2 (text "except perhaps to import instances from"
                                   <+> quotes pp_mod)
                , text "To import instances alone, use:"
                                   <+> text "import" <+> pp_mod <> parens Outputable.empty ]
    msg2 = sep [ pp_herald <+> quotes sort_unused
               , text "from module" <+> quotes pp_mod <+> is_redundant]
    pp_herald  = text "The" <+> pp_qual <+> text "import of"
    pp_qual
      | isImportDeclQualified (ideclQualified decl)= text "qualified"
      | otherwise                                  = Outputable.empty
    pp_mod       = ppr (unLoc (ideclName decl))
    is_redundant = text "is redundant"

    -- In warning message, pretty-print identifiers unqualified unconditionally
    -- to improve the consistent for ambiguous/unambiguous identifiers.
    -- See trac#14881.
    ppr_possible_field n = case lookupNameEnv fld_env n of
                               Just (fld, ParentIs p) -> pprNameUnqualified p <> parens (ppr fld)
                               Just (fld, NoParent)   -> ppr fld
                               Nothing                -> pprNameUnqualified n

    -- Print unused names in a deterministic (lexicographic) order
    sort_unused :: SDoc
    sort_unused = pprWithCommas ppr_possible_field $
                  sortBy (comparing nameOccName) unused

{-
Note [Do not warn about Prelude hiding]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We do not warn about
   import Prelude hiding( x, y )
because even if nothing else from Prelude is used, it may be essential to hide
x,y to avoid name-shadowing warnings.  Example (#9061)
   import Prelude hiding( log )
   f x = log where log = ()



Note [Printing minimal imports]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To print the minimal imports we walk over the user-supplied import
decls, and simply trim their import lists.  NB that

  * We do *not* change the 'qualified' or 'as' parts!

  * We do not discard a decl altogether; we might need instances
    from it.  Instead we just trim to an empty import list
-}

getMinimalImports :: [ImportDeclUsage] -> RnM [LImportDecl GhcRn]
getMinimalImports = fmap combine . mapM mk_minimal
  where
    mk_minimal (L l decl, used_gres, unused)
      | null unused
      , Just (False, _) <- ideclHiding decl
      = return (L l decl)
      | otherwise
      = do { let ImportDecl { ideclName    = L _ mod_name
                            , ideclSource  = is_boot
                            , ideclPkgQual = pkg_qual } = decl
           ; iface <- loadSrcInterface doc mod_name is_boot pkg_qual
           ; let used_avails = gresToAvailInfo used_gres
                 lies = map (L l) (concatMap (to_ie iface) used_avails)
           ; return (L l (decl { ideclHiding = Just (False, L (l2l l) lies) })) }
      where
        doc = text "Compute minimal imports for" <+> ppr decl

    to_ie :: ModIface -> AvailInfo -> [IE GhcRn]
    -- The main trick here is that if we're importing all the constructors
    -- we want to say "T(..)", but if we're importing only a subset we want
    -- to say "T(A,B,C)".  So we have to find out what the module exports.
    to_ie _ (Avail c)  -- Note [Overloaded field import]
       = [IEVar noExtField (to_ie_post_rn $ noLocA (greNamePrintableName c))]
    to_ie _ avail@(AvailTC n [_])  -- Exporting the main decl and nothing else
       | availExportsDecl avail = [IEThingAbs noAnn (to_ie_post_rn $ noLocA n)]
    to_ie iface (AvailTC n cs)
      = case [xs | avail@(AvailTC x xs) <- mi_exports iface
                 , x == n
                 , availExportsDecl avail  -- Note [Partial export]
                 ] of
           [xs] | all_used xs ->
                   [IEThingAll noAnn (to_ie_post_rn $ noLocA n)]
                | otherwise   ->
                   [IEThingWith (map noLoc fs) (to_ie_post_rn $ noLocA n) NoIEWildcard
                                (map (to_ie_post_rn . noLocA) (filter (/= n) ns))]
                                          -- Note [Overloaded field import]
           _other | all_non_overloaded fs
                           -> map (IEVar noExtField . to_ie_post_rn_var . noLocA) $ ns
                                 ++ map flSelector fs
                  | otherwise ->
                      [IEThingWith (map noLoc fs) (to_ie_post_rn $ noLocA n) NoIEWildcard
                                (map (to_ie_post_rn . noLocA) (filter (/= n) ns))]
        where
          (ns, fs) = partitionGreNames cs

          all_used avail_cs = all (`elem` cs) avail_cs

          all_non_overloaded = all (not . flIsOverloaded)

    combine :: [LImportDecl GhcRn] -> [LImportDecl GhcRn]
    combine = map merge . groupBy ((==) `on` getKey) . sortOn getKey

    getKey :: LImportDecl GhcRn -> (Bool, Maybe ModuleName, ModuleName)
    getKey decl =
      ( isImportDeclQualified . ideclQualified $ idecl -- is this qualified? (important that this be first)
      , unLoc <$> ideclAs idecl -- what is the qualifier (inside Maybe monad)
      , unLoc . ideclName $ idecl -- Module Name
      )
      where
        idecl :: ImportDecl GhcRn
        idecl = unLoc decl

    merge :: [LImportDecl GhcRn] -> LImportDecl GhcRn
    merge []                     = error "getMinimalImports: unexpected empty list"
    merge decls@((L l decl) : _) = L l (decl { ideclHiding = Just (False, L (noAnnSrcSpan (locA l)) lies) })
      where lies = concatMap (unLoc . snd) $ mapMaybe (ideclHiding . unLoc) decls


printMinimalImports :: HscSource -> [ImportDeclUsage] -> RnM ()
-- See Note [Printing minimal imports]
printMinimalImports hsc_src imports_w_usage
  = do { imports' <- getMinimalImports imports_w_usage
       ; this_mod <- getModule
       ; dflags   <- getDynFlags
       ; liftIO $ withFile (mkFilename dflags this_mod) WriteMode $ \h ->
          printForUser dflags h neverQualify AllTheWay (vcat (map ppr imports'))
              -- The neverQualify is important.  We are printing Names
              -- but they are in the context of an 'import' decl, and
              -- we never qualify things inside there
              -- E.g.   import Blag( f, b )
              -- not    import Blag( Blag.f, Blag.g )!
       }
  where
    mkFilename dflags this_mod
      | Just d <- dumpDir dflags = d </> basefn
      | otherwise                = basefn
      where
        suffix = case hsc_src of
                     HsBootFile -> ".imports-boot"
                     HsSrcFile  -> ".imports"
                     HsigFile   -> ".imports"
        basefn = moduleNameString (moduleName this_mod) ++ suffix


to_ie_post_rn_var :: (HasOccName name) => LocatedA name -> LIEWrappedName name
to_ie_post_rn_var (L l n)
  | isDataOcc $ occName n = L l (IEPattern (EpaSpan $ la2r l) (L (la2na l) n))
  | otherwise             = L l (IEName                       (L (la2na l) n))


to_ie_post_rn :: (HasOccName name) => LocatedA name -> LIEWrappedName name
to_ie_post_rn (L l n)
  | isTcOcc occ && isSymOcc occ = L l (IEType (EpaSpan $ la2r l) (L (la2na l) n))
  | otherwise                   = L l (IEName                    (L (la2na l) n))
  where occ = occName n

{-
Note [Partial export]
~~~~~~~~~~~~~~~~~~~~~
Suppose we have

   module A( op ) where
     class C a where
       op :: a -> a

   module B where
   import A
   f = ..op...

Then the minimal import for module B is
   import A( op )
not
   import A( C( op ) )
which we would usually generate if C was exported from B.  Hence
the availExportsDecl test when deciding what to generate.


Note [Overloaded field import]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
On the other hand, if we have

    {-# LANGUAGE DuplicateRecordFields #-}
    module A where
      data T = MkT { foo :: Int }

    module B where
      import A
      f = ...foo...

then the minimal import for module B must be
    import A ( T(foo) )
because when DuplicateRecordFields is enabled, field selectors are
not in scope without their enclosing datatype.

On the third hand, if we have

    {-# LANGUAGE DuplicateRecordFields #-}
    module A where
      pattern MkT { foo } = Just foo

    module B where
      import A
      f = ...foo...

then the minimal import for module B must be
    import A ( foo )
because foo doesn't have a parent.  This might actually be ambiguous if A
exports another field called foo, but there is no good answer to return and this
is a very obscure corner, so it seems to be the best we can do.  See
DRFPatSynExport for a test of this.


************************************************************************
*                                                                      *
\subsection{Errors}
*                                                                      *
************************************************************************
-}

qualImportItemErr :: RdrName -> SDoc
qualImportItemErr rdr
  = hang (text "Illegal qualified name in import item:")
       2 (ppr rdr)

ambiguousImportItemErr :: RdrName -> [AvailInfo] -> SDoc
ambiguousImportItemErr rdr avails
  = hang (text "Ambiguous name" <+> quotes (ppr rdr) <+> text "in import item. It could refer to:")
       2 (vcat (map ppr_avail avails))
  where
    ppr_avail (AvailTC parent _) = ppr parent <> parens (ppr rdr)
    ppr_avail (Avail name)       = ppr name

pprImpDeclSpec :: ModIface -> ImpDeclSpec -> SDoc
pprImpDeclSpec iface decl_spec =
  quotes (ppr (is_mod decl_spec)) <+> case mi_boot iface of
    IsBoot -> text "(hi-boot interface)"
    NotBoot -> Outputable.empty

badImportItemErrStd :: ModIface -> ImpDeclSpec -> IE GhcPs -> SDoc
badImportItemErrStd iface decl_spec ie
  = sep [text "Module", pprImpDeclSpec iface decl_spec,
         text "does not export", quotes (ppr ie)]

badImportItemErrDataCon :: OccName -> ModIface -> ImpDeclSpec -> IE GhcPs
                        -> SDoc
badImportItemErrDataCon dataType_occ iface decl_spec ie
  = vcat [ text "In module"
             <+> pprImpDeclSpec iface decl_spec
             <> colon
         , nest 2 $ quotes datacon
             <+> text "is a data constructor of"
             <+> quotes dataType
         , text "To import it use"
         , nest 2 $ text "import"
             <+> ppr (is_mod decl_spec)
             <> parens_sp (dataType <> parens_sp datacon)
         , text "or"
         , nest 2 $ text "import"
             <+> ppr (is_mod decl_spec)
             <> parens_sp (dataType <> text "(..)")
         ]
  where
    datacon_occ = rdrNameOcc $ ieName ie
    datacon = parenSymOcc datacon_occ (ppr datacon_occ)
    dataType = parenSymOcc dataType_occ (ppr dataType_occ)
    parens_sp d = parens (space <> d <> space)  -- T( f,g )

badImportItemErr :: ModIface -> ImpDeclSpec -> IE GhcPs -> [AvailInfo] -> SDoc
badImportItemErr iface decl_spec ie avails
  = case find checkIfDataCon avails of
      Just con -> badImportItemErrDataCon (availOccName con) iface decl_spec ie
      Nothing  -> badImportItemErrStd iface decl_spec ie
  where
    checkIfDataCon (AvailTC _ ns) =
      case find (\n -> importedFS == occNameFS (occName n)) ns of
        Just n  -> isDataConName (greNameMangledName n)
        Nothing -> False
    checkIfDataCon _ = False
    availOccName = occName . availGreName
    importedFS = occNameFS . rdrNameOcc $ ieName ie

illegalImportItemErr :: SDoc
illegalImportItemErr = text "Illegal import item"

addDupDeclErr :: [GlobalRdrElt] -> TcRn ()
addDupDeclErr [] = panic "addDupDeclErr: empty list"
addDupDeclErr gres@(gre : _)
  = addErrAt (getSrcSpan (last sorted_names)) $ TcRnUnknownMessage $ mkPlainError noHints $
    -- Report the error at the later location
    vcat [text "Multiple declarations of" <+>
             quotes (ppr (greOccName gre)),
             -- NB. print the OccName, not the Name, because the
             -- latter might not be in scope in the RdrEnv and so will
             -- be printed qualified.
          text "Declared at:" <+>
                   vcat (map (ppr . nameSrcLoc) sorted_names)]
  where
    sorted_names =
      sortBy (SrcLoc.leftmost_smallest `on` nameSrcSpan)
             (map greMangledName gres)



missingImportListWarn :: ModuleName -> SDoc
missingImportListWarn mod
  = text "The module" <+> quotes (ppr mod) <+> text "does not have an explicit import list"

moduleWarn :: ModuleName -> WarningTxt -> SDoc
moduleWarn mod (WarningTxt _ txt)
  = sep [ text "Module" <+> quotes (ppr mod) <> colon,
          nest 2 (vcat (map (ppr . sl_fs . unLoc) txt)) ]
moduleWarn mod (DeprecatedTxt _ txt)
  = sep [ text "Module" <+> quotes (ppr mod)
                                <+> text "is deprecated:",
          nest 2 (vcat (map (ppr . sl_fs . unLoc) txt)) ]

packageImportErr :: TcRnMessage
packageImportErr
  = TcRnUnknownMessage $ mkPlainError noHints $
  text "Package-qualified imports are not enabled; use PackageImports"

-- This data decl will parse OK
--      data T = a Int
-- treating "a" as the constructor.
-- It is really hard to make the parser spot this malformation.
-- So the renamer has to check that the constructor is legal
--
-- We can get an operator as the constructor, even in the prefix form:
--      data T = :% Int Int
-- from interface files, which always print in prefix form

checkConName :: RdrName -> TcRn ()
checkConName name = checkErr (isRdrDataCon name) (badDataCon name)

badDataCon :: RdrName -> TcRnMessage
badDataCon name
   = TcRnUnknownMessage $ mkPlainError noHints $
   hsep [text "Illegal data constructor name", quotes (ppr name)]
