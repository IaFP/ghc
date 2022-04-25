

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module GHC.HsToCore.Usage (
    -- * Dependency/fingerprinting code (used by GHC.Iface.Make)
    mkUsageInfo, mkUsedNames,
    ) where

import GHC.Prelude

import GHC.Driver.Env
import GHC.Driver.Session


import GHC.Tc.Types

import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Fingerprint
import GHC.Utils.Panic

import GHC.Types.Name
import GHC.Types.Name.Set ( NameSet, allUses )
import GHC.Types.Unique.Set

import GHC.Unit
import GHC.Unit.External
import GHC.Unit.Module.Imported
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.Deps

import GHC.Data.Maybe

import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map as Map

import GHC.Linker.Types
import GHC.Linker.Loader ( getLoaderState )
import GHC.Types.SourceFile

{- Note [Module self-dependency]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GHC.Rename.Names.calculateAvails asserts the invariant that a module must not occur in
its own dep_orphs or dep_finsts. However, if we aren't careful this can occur
in the presence of hs-boot files: Consider that we have two modules, A and B,
both with hs-boot files,

    A.hs contains a SOURCE import of B B.hs-boot contains a SOURCE import of A
    A.hs-boot declares an orphan instance A.hs defines the orphan instance

In this case, B's dep_orphs will contain A due to its SOURCE import of A.
Consequently, A will contain itself in its imp_orphs due to its import of B.
This fact would end up being recorded in A's interface file. This would then
break the invariant asserted by calculateAvails that a module does not itself in
its dep_orphs. This was the cause of #14128.

-}

mkUsedNames :: TcGblEnv -> NameSet
mkUsedNames TcGblEnv{ tcg_dus = dus } = allUses dus

mkUsageInfo :: HscEnv -> Module -> HscSource -> ImportedMods -> NameSet -> [FilePath]
            -> [(Module, Fingerprint)] -> IO [Usage]
mkUsageInfo hsc_env this_mod src dir_imp_mods used_names dependent_files merged
  = do
    eps <- hscEPS hsc_env
    hashes <- mapM getFileHash dependent_files
    -- Dependencies on object files due to TH and plugins
    object_usages <- mkObjectUsage (eps_PIT eps) hsc_env (GWIB (moduleName this_mod) (hscSourceToIsBoot src))
    let mod_usages = mk_mod_usage_info (eps_PIT eps) hsc_env this_mod
                                       dir_imp_mods used_names
        usages = mod_usages ++ [ UsageFile { usg_file_path = f
                                           , usg_file_hash = hash
                                           , usg_file_label = Nothing }
                               | (f, hash) <- zip dependent_files hashes ]
                            ++ [ UsageMergedRequirement
                                    { usg_mod = mod,
                                      usg_mod_hash = hash
                                    }
                               | (mod, hash) <- merged ]
                            ++ object_usages
    usages `seqList` return usages
    -- seq the list of Usages returned: occasionally these
    -- don't get evaluated for a while and we can end up hanging on to
    -- the entire collection of Ifaces.

{- Note [Plugin dependencies]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~

Modules for which plugins were used in the compilation process, should be
recompiled whenever one of those plugins changes. But how do we know if a
plugin changed from the previous time a module was compiled?

We could try storing the fingerprints of the interface files of plugins in
the interface file of the module. And see if there are changes between
compilation runs. However, this is pretty much a non-option because interface
fingerprints of plugin modules are fairly stable, unless you compile plugins
with optimisations turned on, and give basically all binders an INLINE pragma.

So instead:

  * For plugins that were built locally: we store the filepath and hash of the
    object files of the module with the `plugin` binder, and the object files of
    modules that are dependencies of the plugin module and belong to the same
    `UnitId` as the plugin
  * For plugins in an external package: we store the filepath and hash of
    the dynamic library containing the plugin module.

During recompilation we then compare the hashes of those files again to see
if anything has changed.

One issue with this approach is that object files are currently (GHC 8.6.1)
not created fully deterministically, which could sometimes induce accidental
recompilation of a module for which plugins were used in the compile process.

One way to improve this is to either:

  * Have deterministic object file creation
  * Create and store implementation hashes, which would be based on the Core
    of the module and the implementation hashes of its dependencies, and then
    compare implementation hashes for recompilation. Creation of implementation
    hashes is however potentially expensive.
-}

-- | Find object files corresponding to the transitive closure of given home
-- modules and direct object files for pkg dependencies
mkObjectUsage :: PackageIfaceTable -> HscEnv -> ModuleNameWithIsBoot -> IO [Usage]
mkObjectUsage pit hsc_env mnwib = do
  case hsc_interp hsc_env of
      Just interp -> do
        mps <- getLoaderState interp
        case mps of
          Just ps -> do
            let ls = fromMaybe [] $ Map.lookup mnwib (module_deps ps)
                ds = hs_objs_loaded ps
            concat <$> sequence (map linkableToUsage ls ++ map librarySpecToUsage ds)
          Nothing -> return []
      Nothing -> return []


  where
    linkableToUsage (LM _ m uls) = mapM (unlinkedToUsage m) uls

    msg m = moduleNameString (moduleName m) ++ "[TH] changed"

    fing mmsg fn = UsageFile fn <$> getFileHash fn <*> pure mmsg

    unlinkedToUsage m ul =
      case nameOfObject_maybe ul of
        Just fn -> fing (Just (msg m)) fn
        Nothing ->  do
          -- This should only happen for home package things but oneshot puts
          -- home package ifaces in the PIT.
          let miface = lookupIfaceByModule (hsc_HPT hsc_env) pit m
          case miface of
            Nothing -> pprPanic "mkObjectUsage" (ppr m)
            Just iface ->
              return $ UsageHomeModuleInterface (moduleName m) (mi_iface_hash (mi_final_exts iface))

    librarySpecToUsage :: LibrarySpec -> IO [Usage]
    librarySpecToUsage (Objects os) = traverse (fing Nothing) os
    librarySpecToUsage (Archive fn) = traverse (fing Nothing) [fn]
    librarySpecToUsage (DLLPath fn) = traverse (fing Nothing) [fn]
    librarySpecToUsage _ = return []

mk_mod_usage_info :: PackageIfaceTable
              -> HscEnv
              -> Module
              -> ImportedMods
              -> NameSet
              -> [Usage]
mk_mod_usage_info pit hsc_env this_mod direct_imports used_names
  = mapMaybe mkUsage usage_mods
  where
    hpt = hsc_HPT hsc_env
    dflags = hsc_dflags hsc_env
    home_unit = hsc_home_unit hsc_env

    used_mods    = moduleEnvKeys ent_map
    dir_imp_mods = moduleEnvKeys direct_imports
    all_mods     = used_mods ++ filter (`notElem` used_mods) dir_imp_mods
    usage_mods   = sortBy stableModuleCmp all_mods
                        -- canonical order is imported, to avoid interface-file
                        -- wobblage.

    -- ent_map groups together all the things imported and used
    -- from a particular module
    ent_map :: ModuleEnv [OccName]
    ent_map  = nonDetStrictFoldUniqSet add_mv emptyModuleEnv used_names
     -- nonDetStrictFoldUniqSet is OK here. If you follow the logic, we sort by
     -- OccName in ent_hashs
     where
      add_mv name mv_map
        | isWiredInName name = mv_map  -- ignore wired-in names
        | otherwise
        = case nameModule_maybe name of
             Nothing  -> assertPpr (isSystemName name) (ppr name) mv_map
                -- See Note [Internal used_names]

             Just mod ->
                -- See Note [Identity versus semantic module]
                let mod' = if isHoleModule mod
                            then mkHomeModule home_unit (moduleName mod)
                            else mod
                -- This lambda function is really just a
                -- specialised (++); originally came about to
                -- avoid quadratic behaviour (trac #2680)
                in extendModuleEnvWith (\_ xs -> occ:xs) mv_map mod' [occ]
            where occ = nameOccName name

    -- We want to create a Usage for a home module if
    --  a) we used something from it; has something in used_names
    --  b) we imported it, even if we used nothing from it
    --     (need to recompile if its export list changes: export_fprint)
    mkUsage :: Module -> Maybe Usage
    mkUsage mod
      | isNothing maybe_iface           -- We can't depend on it if we didn't
                                        -- load its interface.
      || mod == this_mod                -- We don't care about usages of
                                        -- things in *this* module
      = Nothing

      | not (isHomeModule home_unit mod)
      = Just UsagePackageModule{ usg_mod      = mod,
                                 usg_mod_hash = mod_hash,
                                 usg_safe     = imp_safe }
        -- for package modules, we record the module hash only

      | (null used_occs
          && isNothing export_hash
          && not is_direct_import
          && not finsts_mod)
      = Nothing                 -- Record no usage info
        -- for directly-imported modules, we always want to record a usage
        -- on the orphan hash.  This is what triggers a recompilation if
        -- an orphan is added or removed somewhere below us in the future.

      | otherwise
      = Just UsageHomeModule {
                      usg_mod_name = moduleName mod,
                      usg_mod_hash = mod_hash,
                      usg_exports  = export_hash,
                      usg_entities = Map.toList ent_hashs,
                      usg_safe     = imp_safe }
      where
        maybe_iface  = lookupIfaceByModule hpt pit mod
                -- In one-shot mode, the interfaces for home-package
                -- modules accumulate in the PIT not HPT.  Sigh.

        Just iface   = maybe_iface
        finsts_mod   = mi_finsts (mi_final_exts iface)
        hash_env     = mi_hash_fn (mi_final_exts iface)
        mod_hash     = mi_mod_hash (mi_final_exts iface)
        export_hash | depend_on_exports = Just (mi_exp_hash (mi_final_exts iface))
                    | otherwise         = Nothing

        by_is_safe (ImportedByUser imv) = imv_is_safe imv
        by_is_safe _ = False
        (is_direct_import, imp_safe)
            = case lookupModuleEnv direct_imports mod of
                -- ezyang: I'm not sure if any is the correct
                -- metric here. If safety was guaranteed to be uniform
                -- across all imports, why did the old code only look
                -- at the first import?
                Just bys -> (True, any by_is_safe bys)
                Nothing  -> (False, safeImplicitImpsReq dflags)
                -- Nothing case is for references to entities which were
                -- not directly imported (NB: the "implicit" Prelude import
                -- counts as directly imported!  An entity is not directly
                -- imported if, e.g., we got a reference to it from a
                -- reexport of another module.)

        used_occs = lookupModuleEnv ent_map mod `orElse` []

        -- Making a Map here ensures that (a) we remove duplicates
        -- when we have usages on several subordinates of a single parent,
        -- and (b) that the usages emerge in a canonical order, which
        -- is why we use Map rather than OccEnv: Map works
        -- using Ord on the OccNames, which is a lexicographic ordering.
        ent_hashs :: Map OccName Fingerprint
        ent_hashs = Map.fromList $ catMaybes (map lookup_occ used_occs)
      
        lookup_occ :: OccName -> Maybe (OccName, Fingerprint)
        -- There migh be names that don't have occurances due to building of WF_*
        lookup_occ occ
          | isJust ochs 
          = ochs
          | Nothing <- ochs
          , isWFTyConOcc occ
          = Nothing
          | otherwise
          = pprPanic "mkUsage" (ppr mod <+> ppr occ <+> ppr used_names)
          where
            ochs = hash_env occ

        depend_on_exports = is_direct_import
        {- True
              Even if we used 'import M ()', we have to register a
              usage on the export list because we are sensitive to
              changes in orphan instances/rules.
           False
              In GHC 6.8.x we always returned true, and in
              fact it recorded a dependency on *all* the
              modules underneath in the dependency tree.  This
              happens to make orphans work right, but is too
              expensive: it'll read too many interface files.
              The 'isNothing maybe_iface' check above saved us
              from generating many of these usages (at least in
              one-shot mode), but that's even more bogus!
        -}

{-
Note [Internal used_names]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Most of the used_names are External Names, but we can have System
Names too. Two examples:

* Names arising from Language.Haskell.TH.newName.
  See Note [Binders in Template Haskell] in GHC.ThToHs (and #5362).
* The names of auxiliary bindings in derived instances.
  See Note [Auxiliary binders] in GHC.Tc.Deriv.Generate.

Such Names are always for locally-defined things, for which we don't gather
usage info, so we can just ignore them in ent_map. Moreover, they are always
System Names, hence the assert, just as a double check.
-}
