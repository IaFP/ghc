{-# LANGUAGE CPP #-}
{-# LANGUAGE NondecreasingIndentation, ScopedTypeVariables #-}
{-# LANGUAGE TupleSections, NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
#if __GLASGOW_HASKELL__ >= 903
{-# LANGUAGE QuantifiedConstraints, ExplicitNamespaces, TypeOperators #-}
#endif

-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow, 2005-2012
--
-- The GHC API
--
-- -----------------------------------------------------------------------------

module GHC (
        -- * Initialisation
        defaultErrorHandler,
        -- defaultCleanupHandler,
        prettyPrintGhcErrors,
        withSignalHandlers,
        withCleanupSession,

        -- * GHC Monad
        Ghc, GhcT, GhcMonad(..), HscEnv,
        runGhc, runGhcT, initGhcMonad,
        printException,
        handleSourceError,
        needsTemplateHaskellOrQQ,

        -- * Flags and settings
        DynFlags(..), GeneralFlag(..), Severity(..), Backend(..), gopt,
        GhcMode(..), GhcLink(..),
        parseDynamicFlags, parseTargetFiles,
        getSessionDynFlags, setSessionDynFlags,
        getProgramDynFlags, setProgramDynFlags,
        getInteractiveDynFlags, setInteractiveDynFlags,
        interpretPackageEnv,

        -- * Logging
        Logger, getLogger,
        pushLogHook, popLogHook,
        pushLogHookM, popLogHookM, modifyLogger,
        putMsgM, putLogMsgM,


        -- * Targets
        Target(..), TargetId(..), Phase,
        setTargets,
        getTargets,
        addTarget,
        removeTarget,
        guessTarget,

        -- * Loading\/compiling the program
        depanal, depanalE,
        load, loadWithCache, LoadHowMuch(..), InteractiveImport(..),
        SuccessFlag(..), succeeded, failed,
        defaultWarnErrLogger, WarnErrLogger,
        workingDirectoryChanged,
        parseModule, typecheckModule, desugarModule,
        ParsedModule(..), TypecheckedModule(..), DesugaredModule(..),
        TypecheckedSource, ParsedSource, RenamedSource,   -- ditto
        TypecheckedMod, ParsedMod,
        moduleInfo, renamedSource, typecheckedSource,
        parsedSource, coreModule,
        PkgQual(..),

        -- ** Compiling to Core
        CoreModule(..),
        compileToCoreModule, compileToCoreSimplified,

        -- * Inspecting the module structure of the program
        ModuleGraph, emptyMG, mapMG, mkModuleGraph, mgModSummaries,
        mgLookupModule,
        ModSummary(..), ms_mod_name, ModLocation(..),
        getModSummary,
        getModuleGraph,
        isLoaded,
        topSortModuleGraph,

        -- * Inspecting modules
        ModuleInfo,
        getModuleInfo,
        modInfoTyThings,
        modInfoTopLevelScope,
        modInfoExports,
        modInfoExportsWithSelectors,
        modInfoInstances,
        modInfoIsExportedName,
        modInfoLookupName,
        modInfoIface,
        modInfoRdrEnv,
        modInfoSafe,
        lookupGlobalName,
        findGlobalAnns,
        mkPrintUnqualifiedForModule,
        ModIface, ModIface_(..),
        SafeHaskellMode(..),

        -- * Printing
        PrintUnqualified, alwaysQualify,

        -- * Interactive evaluation

        -- ** Executing statements
        execStmt, execStmt', ExecOptions(..), execOptions, ExecResult(..),
        resumeExec,

        -- ** Adding new declarations
        runDecls, runDeclsWithLocation, runParsedDecls,

        -- ** Get/set the current context
        parseImportDecl,
        setContext, getContext,
        setGHCiMonad, getGHCiMonad,

        -- ** Inspecting the current context
        getBindings, getInsts, getPrintUnqual,
        findModule, lookupModule,
        findQualifiedModule, lookupQualifiedModule,
        renamePkgQualM, renameRawPkgQualM,
        isModuleTrusted, moduleTrustReqs,
        getNamesInScope,
        getRdrNamesInScope,
        getGRE,
        moduleIsInterpreted,
        getInfo,
        showModule,
        moduleIsBootOrNotObjectLinkable,
        getNameToInstancesIndex,

        -- ** Inspecting types and kinds
        exprType, TcRnExprMode(..),
        typeKind,

        -- ** Looking up a Name
        parseName,
        lookupName,

        -- ** Compiling expressions
        HValue, parseExpr, compileParsedExpr,
        GHC.Runtime.Eval.compileExpr, dynCompileExpr,
        ForeignHValue,
        compileExprRemote, compileParsedExprRemote,

        -- ** Docs
        getDocs, GetDocsFailure(..),

        -- ** Other
        runTcInteractive,   -- Desired by some clients (#8878)
        isStmt, hasImport, isImport, isDecl,

        -- ** The debugger
        SingleStep(..),
        Resume(..),
        History(historyBreakInfo, historyEnclosingDecls),
        GHC.getHistorySpan, getHistoryModule,
        abandon, abandonAll,
        getResumeContext,
        GHC.obtainTermFromId, GHC.obtainTermFromVal, reconstructType,
        modInfoModBreaks,
        ModBreaks(..), BreakIndex,
        BreakInfo(..),
        GHC.Runtime.Eval.back,
        GHC.Runtime.Eval.forward,
        GHC.Runtime.Eval.setupBreakpoint,

        -- * Abstract syntax elements

        -- ** Units
        Unit,

        -- ** Modules
        Module, mkModule, pprModule, moduleName, moduleUnit,
        ModuleName, mkModuleName, moduleNameString,

        -- ** Names
        Name,
        isExternalName, nameModule, pprParenSymName, nameSrcSpan,
        NamedThing(..),
        RdrName(Qual,Unqual),

        -- ** Identifiers
        Id, idType,
        isImplicitId, isDeadBinder,
        isExportedId, isLocalId, isGlobalId,
        isRecordSelector,
        isPrimOpId, isFCallId, isClassOpId_maybe,
        isDataConWorkId, idDataCon,
        isDeadEndId, isDictonaryId,
        recordSelectorTyCon,

        -- ** Type constructors
        TyCon,
        tyConTyVars, tyConDataCons, tyConArity,
        isClassTyCon, isTypeSynonymTyCon, isTypeFamilyTyCon, isNewTyCon,
        isPrimTyCon, isFunTyCon,
        isFamilyTyCon, isOpenFamilyTyCon, isOpenTypeFamilyTyCon,
        tyConClass_maybe,
        synTyConRhs_maybe, synTyConDefn_maybe, tyConKind,

        -- ** Type variables
        TyVar,
        alphaTyVars,

        -- ** Data constructors
        DataCon,
        dataConType, dataConTyCon, dataConFieldLabels,
        dataConIsInfix, isVanillaDataCon, dataConWrapperType,
        dataConSrcBangs,
        StrictnessMark(..), isMarkedStrict,

        -- ** Classes
        Class,
        classMethods, classSCTheta, classTvsFds, classATs,
        pprFundeps,

        -- ** Instances
        ClsInst,
        instanceDFunId,
        pprInstance, pprInstanceHdr,
        pprFamInst,

        FamInst,

        -- ** Types and Kinds
        Type, splitForAllTyCoVars, funResultTy,
        pprParendType, pprTypeApp,
        Kind,
        PredType,
        ThetaType, pprForAll, pprThetaArrowTy,
        parseInstanceHead,
        getInstancesForType,

        -- ** Entities
        TyThing(..),

        -- ** Syntax
        module GHC.Hs, -- ToDo: remove extraneous bits

        -- ** Fixities
        FixityDirection(..),
        defaultFixity, maxPrecedence,
        negateFixity,
        compareFixity,
        LexicalFixity(..),

        -- ** Source locations
        SrcLoc(..), RealSrcLoc,
        mkSrcLoc, noSrcLoc,
        srcLocFile, srcLocLine, srcLocCol,
        SrcSpan(..), RealSrcSpan,
        mkSrcSpan, srcLocSpan, isGoodSrcSpan, noSrcSpan,
        srcSpanStart, srcSpanEnd,
        srcSpanFile,
        srcSpanStartLine, srcSpanEndLine,
        srcSpanStartCol, srcSpanEndCol,

        -- ** Located
        GenLocated(..), Located, RealLocated,

        -- *** Constructing Located
        noLoc, mkGeneralLocated,

        -- *** Deconstructing Located
        getLoc, unLoc,
        getRealSrcSpan, unRealSrcSpan,

        -- *** Combining and comparing Located values
        eqLocated, cmpLocated, combineLocs, addCLoc,
        leftmost_smallest, leftmost_largest, rightmost_smallest,
        spans, isSubspanOf,

        -- * Exceptions
        GhcException(..), showGhcException,
        GhcApiError(..),

        -- * Token stream manipulations
        Token,
        getTokenStream, getRichTokenStream,
        showRichTokenStream, addSourceToTokens,

        -- * Pure interface to the parser
        parser,

        -- * API Annotations
        AnnKeywordId(..),EpaComment(..),

        -- * Miscellaneous
        --sessionHscEnv,
        cyclicModuleErr,
  ) where

{-
 ToDo:

  * inline bits of GHC.Driver.Main here to simplify layering: hscTcExpr, hscStmt.
-}

import GHC.Prelude hiding (init)

import GHC.Platform
import GHC.Platform.Ways

import GHC.Driver.Phases   ( Phase(..), isHaskellSrcFilename
                           , isSourceFilename, startPhase )
import GHC.Driver.Env
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Driver.CmdLine
import GHC.Driver.Session
import GHC.Driver.Backend
import GHC.Driver.Config.Finder (initFinderOpts)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Driver.Config.Logger (initLogFlags)
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Main
import GHC.Driver.Make
import GHC.Driver.Hooks
import GHC.Driver.Monad
import GHC.Driver.Ppr

import GHC.ByteCode.Types
import qualified GHC.Linker.Loader as Loader
import GHC.Runtime.Loader
import GHC.Runtime.Eval
import GHC.Runtime.Interpreter
import GHC.Runtime.Context
import GHCi.RemoteTypes

import qualified GHC.Parser as Parser
import GHC.Parser.Lexer
import GHC.Parser.Annotation
import GHC.Parser.Utils

import GHC.Iface.Load        ( loadSysInterface )
import GHC.Hs
import GHC.Builtin.Types.Prim ( alphaTyVars )
import GHC.Iface.Tidy
import GHC.Data.StringBuffer
import GHC.Data.FastString
import qualified GHC.LanguageExtensions as LangExt
import GHC.Rename.Names (renamePkgQual, renameRawPkgQual)

import GHC.Tc.Utils.Monad    ( finalSafeMode, fixSafeInstances, initIfaceTcRn )
import GHC.Tc.Types
import GHC.Tc.Utils.TcType
import GHC.Tc.Module
import GHC.Tc.Utils.Instantiate
import GHC.Tc.Instance.Family

import GHC.Utils.TmpFs
import GHC.SysTools
import GHC.SysTools.BaseDir

import GHC.Utils.Error
import GHC.Utils.Monad
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Logger
import GHC.Utils.Fingerprint

import GHC.Core.Predicate
import GHC.Core.Type  hiding( typeKind )
import GHC.Core.TyCon
import GHC.Core.TyCo.Ppr   ( pprForAll )
import GHC.Core.Class
import GHC.Core.DataCon
import GHC.Core.FVs        ( orphNamesOfFamInst )
import GHC.Core.FamInstEnv ( FamInst, famInstEnvElts )
import GHC.Core.InstEnv
import GHC.Core

import GHC.Types.Id
import GHC.Types.Name      hiding ( varName )
import GHC.Types.Avail
import GHC.Types.SrcLoc
import GHC.Types.TyThing.Ppr  ( pprFamInst )
import GHC.Types.Annotations
import GHC.Types.Name.Set
import GHC.Types.Name.Reader
import GHC.Types.SourceError
import GHC.Types.SafeHaskell
import GHC.Types.Error
import GHC.Types.Fixity
import GHC.Types.Target
import GHC.Types.Basic
import GHC.Types.TyThing
import GHC.Types.Name.Env
import GHC.Types.Name.Ppr
import GHC.Types.TypeEnv
import GHC.Types.BreakInfo
import GHC.Types.PkgQual

import GHC.Unit
import GHC.Unit.Env
import GHC.Unit.External
import GHC.Unit.Finder
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.ModGuts
import GHC.Unit.Module.ModDetails
import GHC.Unit.Module.ModSummary
import GHC.Unit.Module.Graph
import GHC.Unit.Home.ModInfo

import Data.Foldable
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Sequence as Seq
import Data.Maybe
import Data.Typeable    ( Typeable )
import Data.Word        ( Word8 )
import Control.Monad
import System.Exit      ( exitWith, ExitCode(..) )
import GHC.Utils.Exception
import Data.IORef
import System.FilePath
import Control.Concurrent
import Control.Applicative ((<|>))
import Control.Monad.Catch as MC

import GHC.Data.Maybe
import System.IO.Error  ( isDoesNotExistError )
import System.Environment ( getEnv, getProgName )
import System.Directory
import Data.List (isPrefixOf)


-- %************************************************************************
-- %*                                                                      *
--             Initialisation: exception handlers
-- %*                                                                      *
-- %************************************************************************


-- | Install some default exception handlers and run the inner computation.
-- Unless you want to handle exceptions yourself, you should wrap this around
-- the top level of your program.  The default handlers output the error
-- message(s) to stderr and exit cleanly.
defaultErrorHandler :: (ExceptionMonad m)
                    => FatalMessager -> FlushOut -> m a -> m a
defaultErrorHandler fm (FlushOut flushOut) inner =
  -- top-level exception handler: any unrecognised exception is a compiler bug.
  MC.handle (\exception -> liftIO $ do
           flushOut
           case fromException exception of
                -- an IO exception probably isn't our fault, so don't panic
                Just (ioe :: IOException) ->
                  fm (show ioe)
                _ -> case fromException exception of
                     Just UserInterrupt ->
                         -- Important to let this one propagate out so our
                         -- calling process knows we were interrupted by ^C
                         liftIO $ throwIO UserInterrupt
                     Just StackOverflow ->
                         fm "stack overflow: use +RTS -K<size> to increase it"
                     _ -> case fromException exception of
                          Just (ex :: ExitCode) -> liftIO $ throwIO ex
                          _ ->
                              fm (show (Panic (show exception)))
           exitWith (ExitFailure 1)
         ) $

  -- error messages propagated as exceptions
  handleGhcException
            (\ge -> liftIO $ do
                flushOut
                case ge of
                  Signal _       -> return ()
                  ProgramError _ -> fm (show ge)
                  CmdLineError _ -> fm ("<command line>: " ++ show ge)
                  _              -> do
                                    progName <- getProgName
                                    fm (progName ++ ": " ++ show ge)
                exitWith (ExitFailure 1)
            ) $
  inner

-- | This function is no longer necessary, cleanup is now done by
-- runGhc/runGhcT.
-- {-# DEPRECATED defaultCleanupHandler "Cleanup is now done by runGhc/runGhcT" #-}
-- defaultCleanupHandler :: (Applicative m,ExceptionMonad m) => DynFlags -> m a -> m a
-- defaultCleanupHandler _ m = m
--  where  _warning_suppression = m `MC.onException` undefined


-- %************************************************************************
-- %*                                                                      *
--             The Ghc Monad
-- %*                                                                      *
-- %************************************************************************

-- | Run function for the 'Ghc' monad.
--
-- It initialises the GHC session and warnings via 'initGhcMonad'.  Each call
-- to this function will create a new session which should not be shared among
-- several threads.
--
-- Any errors not handled inside the 'Ghc' action are propagated as IO
-- exceptions.

runGhc :: Maybe FilePath  -- ^ See argument to 'initGhcMonad'.
       -> Ghc a           -- ^ The action to perform.
       -> IO a
runGhc mb_top_dir ghc = do
  ref <- newIORef (panic "empty session")
  let session = Session ref
  flip unGhc session $ withSignalHandlers $ do -- catch ^C
    initGhcMonad mb_top_dir
    withCleanupSession ghc

-- | Run function for 'GhcT' monad transformer.
--
-- It initialises the GHC session and warnings via 'initGhcMonad'.  Each call
-- to this function will create a new session which should not be shared among
-- several threads.

runGhcT :: (ExceptionMonad m) =>
           Maybe FilePath  -- ^ See argument to 'initGhcMonad'.
        -> GhcT m a        -- ^ The action to perform.
        -> m a
runGhcT mb_top_dir ghct = do
  ref <- liftIO $ newIORef (panic "empty session")
  let session = Session ref
  flip unGhcT session $ withSignalHandlers $ do -- catch ^C
    initGhcMonad mb_top_dir
    withCleanupSession ghct

withCleanupSession :: GhcMonad m => m a -> m a
withCleanupSession ghc = ghc `MC.finally` cleanup
  where
   cleanup = do
      hsc_env <- getSession
      let dflags = hsc_dflags hsc_env
      let logger = hsc_logger hsc_env
      let tmpfs  = hsc_tmpfs hsc_env
      liftIO $ do
          unless (gopt Opt_KeepTmpFiles dflags) $ do
            cleanTempFiles logger tmpfs
            cleanTempDirs logger tmpfs
          traverse_ stopInterp (hsc_interp hsc_env)
          --  exceptions will be blocked while we clean the temporary files,
          -- so there shouldn't be any difficulty if we receive further
          -- signals.

-- | Initialise a GHC session.
--
-- If you implement a custom 'GhcMonad' you must call this function in the
-- monad run function.  It will initialise the session variable and clear all
-- warnings.
--
-- The first argument should point to the directory where GHC's library files
-- reside.  More precisely, this should be the output of @ghc --print-libdir@
-- of the version of GHC the module using this API is compiled with.  For
-- portability, you should use the @ghc-paths@ package, available at
-- <http://hackage.haskell.org/package/ghc-paths>.

initGhcMonad :: GhcMonad m => Maybe FilePath -> m ()
initGhcMonad mb_top_dir
  = do { env <- liftIO $
                do { top_dir <- findTopDir mb_top_dir
                   ; mySettings <- initSysTools top_dir
                   ; myLlvmConfig <- lazyInitLlvmConfig top_dir
                   ; dflags <- initDynFlags (defaultDynFlags mySettings myLlvmConfig)
                   ; hsc_env <- newHscEnv dflags
                   ; checkBrokenTablesNextToCode (hsc_logger hsc_env) dflags
                   ; setUnsafeGlobalDynFlags dflags
                      -- c.f. DynFlags.parseDynamicFlagsFull, which
                      -- creates DynFlags and sets the UnsafeGlobalDynFlags
                   ; return hsc_env }
       ; setSession env }

-- | The binutils linker on ARM emits unnecessary R_ARM_COPY relocations which
-- breaks tables-next-to-code in dynamically linked modules. This
-- check should be more selective but there is currently no released
-- version where this bug is fixed.
-- See https://sourceware.org/bugzilla/show_bug.cgi?id=16177 and
-- https://gitlab.haskell.org/ghc/ghc/issues/4210#note_78333
checkBrokenTablesNextToCode :: (MonadIO m) => Logger -> DynFlags -> m ()
checkBrokenTablesNextToCode logger dflags
  = do { broken <- checkBrokenTablesNextToCode' logger dflags
       ; when broken 
         $ do { (_:: ()) <- liftIO $ throwIO $ mkApiErr dflags invalidLdErr
              ; liftIO $ fail "unsupported linker"
              }
       }
  where
    invalidLdErr = text "Tables-next-to-code not supported on ARM" <+>
                   text "when using binutils ld (please see:" <+>
                   text "https://sourceware.org/bugzilla/show_bug.cgi?id=16177)"

checkBrokenTablesNextToCode' :: (MonadIO m) => Logger -> DynFlags -> m Bool
checkBrokenTablesNextToCode' logger dflags
  | not (isARM arch)               = return False
  | ways dflags `hasNotWay` WayDyn = return False
  | not tablesNextToCode           = return False
  | otherwise                      = do
    linkerInfo <- liftIO $ getLinkerInfo logger dflags
    case linkerInfo of
      GnuLD _  -> return True
      _        -> return False
  where platform = targetPlatform dflags
        arch = platformArch platform
        tablesNextToCode = platformTablesNextToCode platform


-- %************************************************************************
-- %*                                                                      *
--             Flags & settings
-- %*                                                                      *
-- %************************************************************************

-- $DynFlags
--
-- The GHC session maintains two sets of 'DynFlags':
--
--   * The "interactive" @DynFlags@, which are used for everything
--     related to interactive evaluation, including 'runStmt',
--     'runDecls', 'exprType', 'lookupName' and so on (everything
--     under \"Interactive evaluation\" in this module).
--
--   * The "program" @DynFlags@, which are used when loading
--     whole modules with 'load'
--
-- 'setInteractiveDynFlags', 'getInteractiveDynFlags' work with the
-- interactive @DynFlags@.
--
-- 'setProgramDynFlags', 'getProgramDynFlags' work with the
-- program @DynFlags@.
--
-- 'setSessionDynFlags' sets both @DynFlags@, and 'getSessionDynFlags'
-- retrieves the program @DynFlags@ (for backwards compatibility).


-- | Updates both the interactive and program DynFlags in a Session.
-- This also reads the package database (unless it has already been
-- read), and prepares the compilers knowledge about packages.  It can
-- be called again to load new packages: just add new package flags to
-- (packageFlags dflags).
setSessionDynFlags :: GhcMonad m => DynFlags -> m ()
setSessionDynFlags dflags0 = do
  logger <- getLogger
  dflags1 <- checkNewDynFlags logger dflags0
  hsc_env <- getSession
  let old_unit_env    = hsc_unit_env hsc_env
  let cached_unit_dbs = ue_unit_dbs old_unit_env
  (dbs,unit_state,home_unit,mconstants) <- liftIO $ initUnits logger dflags1 cached_unit_dbs

  dflags <- liftIO $ updatePlatformConstants dflags1 mconstants

  -- Interpreter
  interp <- if gopt Opt_ExternalInterpreter dflags
    then do
         let
           prog = pgm_i dflags ++ flavour
           profiled = ways dflags `hasWay` WayProf
           dynamic  = ways dflags `hasWay` WayDyn
           flavour
             | profiled  = "-prof" -- FIXME: can't we have both?
             | dynamic   = "-dyn"
             | otherwise = ""
           msg = text "Starting " <> text prog
         tr <- if verbosity dflags >= 3
                then return (logInfo logger $ withPprStyle defaultDumpStyle msg)
                else return (pure ())
         let
          conf = IServConfig
            { iservConfProgram  = prog
            , iservConfOpts     = getOpts dflags opt_i
            , iservConfProfiled = profiled
            , iservConfDynamic  = dynamic
            , iservConfHook     = createIservProcessHook (hsc_hooks hsc_env)
            , iservConfTrace    = tr
            }
         s <- liftIO $ newMVar IServPending
         loader <- liftIO Loader.uninitializedLoader
         return (Just (Interp (ExternalInterp conf (IServ s)) loader))
    else
#if defined(HAVE_INTERNAL_INTERPRETER)
     do
      loader <- liftIO Loader.uninitializedLoader
      return (Just (Interp InternalInterp loader))
#else
      return Nothing
#endif

  let unit_env = UnitEnv
        { ue_platform  = targetPlatform dflags
        , ue_namever   = ghcNameVersion dflags
        , ue_home_unit = Just home_unit
        , ue_hpt       = ue_hpt old_unit_env
        , ue_eps       = ue_eps old_unit_env
        , ue_units     = unit_state
        , ue_unit_dbs  = Just dbs
        }

  modifySession $ \h -> hscSetFlags dflags $
                        h{ hsc_IC = (hsc_IC h){ ic_dflags = dflags }
                         , hsc_interp = hsc_interp h <|> interp
                           -- we only update the interpreter if there wasn't
                           -- already one set up
                         , hsc_unit_env = unit_env
                         }

  invalidateModSummaryCache

-- | Sets the program 'DynFlags'.  Note: this invalidates the internal
-- cached module graph, causing more work to be done the next time
-- 'load' is called.
--
-- Returns a boolean indicating if preload units have changed and need to be
-- reloaded.
setProgramDynFlags :: GhcMonad m => DynFlags -> m Bool
setProgramDynFlags dflags = setProgramDynFlags_ True dflags

setProgramDynFlags_ :: GhcMonad m => Bool -> DynFlags -> m Bool
setProgramDynFlags_ invalidate_needed dflags = do
  logger <- getLogger
  dflags0 <- checkNewDynFlags logger dflags
  dflags_prev <- getProgramDynFlags
  let changed = packageFlagsChanged dflags_prev dflags0
  if changed
    then do
        old_unit_env <- hsc_unit_env <$> getSession
        let cached_unit_dbs = ue_unit_dbs old_unit_env
        (dbs,unit_state,home_unit,mconstants) <- liftIO $ initUnits logger dflags0 cached_unit_dbs

        dflags1 <- liftIO $ updatePlatformConstants dflags0 mconstants

        let unit_env = UnitEnv
              { ue_platform  = targetPlatform dflags1
              , ue_namever   = ghcNameVersion dflags1
              , ue_home_unit = Just home_unit
              , ue_hpt       = ue_hpt old_unit_env
              , ue_eps       = ue_eps old_unit_env
              , ue_units     = unit_state
              , ue_unit_dbs  = Just dbs
              }
        modifySession $ \h -> hscSetFlags dflags1 $ h{ hsc_unit_env = unit_env }
    else modifySession (hscSetFlags dflags0)

  when invalidate_needed $ invalidateModSummaryCache
  return changed


-- When changing the DynFlags, we want the changes to apply to future
-- loads, but without completely discarding the program.  But the
-- DynFlags are cached in each ModSummary in the hsc_mod_graph, so
-- after a change to DynFlags, the changes would apply to new modules
-- but not existing modules; this seems undesirable.
--
-- Furthermore, the GHC API client might expect that changing
-- log_action would affect future compilation messages, but for those
-- modules we have cached ModSummaries for, we'll continue to use the
-- old log_action.  This is definitely wrong (#7478).
--
-- Hence, we invalidate the ModSummary cache after changing the
-- DynFlags.  We do this by tweaking the hash on each ModSummary, so
-- that the next downsweep will think that all the files have changed
-- and preprocess them again.  This won't necessarily cause everything
-- to be recompiled, because by the time we check whether we need to
-- recompile a module, we'll have re-summarised the module and have a
-- correct ModSummary.
--
invalidateModSummaryCache :: GhcMonad m => m ()
invalidateModSummaryCache =
  modifySession $ \h -> h { hsc_mod_graph = mapMG inval (hsc_mod_graph h) }
 where
  inval ms = ms { ms_hs_hash = fingerprint0 }

-- | Returns the program 'DynFlags'.
getProgramDynFlags :: GhcMonad m => m DynFlags
getProgramDynFlags = getSessionDynFlags

-- | Set the 'DynFlags' used to evaluate interactive expressions.
-- Also initialise (load) plugins.
--
-- Note: this cannot be used for changes to packages.  Use
-- 'setSessionDynFlags', or 'setProgramDynFlags' and then copy the
-- 'unitState' into the interactive @DynFlags@.
setInteractiveDynFlags :: GhcMonad m => DynFlags -> m ()
setInteractiveDynFlags dflags = do
  logger <- getLogger
  dflags' <- checkNewDynFlags logger dflags
  dflags'' <- checkNewInteractiveDynFlags logger dflags'
  modifySessionM $ \hsc_env0 -> do
    let ic0 = hsc_IC hsc_env0

    -- Initialise (load) plugins in the interactive environment with the new
    -- DynFlags
    plugin_env <- liftIO $ flip initializePlugins Nothing $ mkInteractiveHscEnv $
                    hsc_env0 { hsc_IC = ic0 { ic_dflags = dflags'' }}

    -- Update both plugins cache and DynFlags in the interactive context.
    return $ hsc_env0
                { hsc_IC = ic0
                    { ic_plugins = hsc_plugins plugin_env
                    , ic_dflags  = hsc_dflags  plugin_env
                    }
                }


-- | Get the 'DynFlags' used to evaluate interactive expressions.
getInteractiveDynFlags :: GhcMonad m => m DynFlags
getInteractiveDynFlags = withSession $ \h -> return (ic_dflags (hsc_IC h))


parseDynamicFlags
    :: (MonadIO m)
    => Logger
    -> DynFlags
    -> [Located String]
    -> m (DynFlags, [Located String], [Warn])
parseDynamicFlags logger dflags cmdline = do
  (dflags1, leftovers, warns) <- parseDynamicFlagsCmdLine dflags cmdline
  -- flags that have just been read are used by the logger when loading package
  -- env (this is checked by T16318)
  let logger1 = setLogFlags logger (initLogFlags dflags1)
  dflags2 <- liftIO $ interpretPackageEnv logger1 dflags1
  return (dflags2, leftovers, warns)

-- | Parse command line arguments that look like files.
-- First normalises its arguments and then splits them into source files
-- and object files.
-- A source file can be turned into a 'Target' via 'guessTarget'
parseTargetFiles :: DynFlags -> [String] -> (DynFlags, [(String, Maybe Phase)], [String])
parseTargetFiles dflags0 fileish_args =
  let
    normal_fileish_paths = map normalise_hyp fileish_args
    (srcs, objs)         = partition_args normal_fileish_paths [] []

    dflags1 = dflags0 { ldInputs = map (FileOption "") objs
                                   ++ ldInputs dflags0 }
    {-
      We split out the object files (.o, .dll) and add them
      to ldInputs for use by the linker.

      The following things should be considered compilation manager inputs:

       - haskell source files (strings ending in .hs, .lhs or other
         haskellish extension),

       - module names (not forgetting hierarchical module names),

       - things beginning with '-' are flags that were not recognised by
         the flag parser, and we want them to generate errors later in
         checkOptions, so we class them as source files (#5921)

       - and finally we consider everything without an extension to be
         a comp manager input, as shorthand for a .hs or .lhs filename.

      Everything else is considered to be a linker object, and passed
      straight through to the linker.
    -}
  in (dflags1, srcs, objs)

-- -----------------------------------------------------------------------------

-- | Splitting arguments into source files and object files.  This is where we
-- interpret the -x <suffix> option, and attach a (Maybe Phase) to each source
-- file indicating the phase specified by the -x option in force, if any.
partition_args :: [String] -> [(String, Maybe Phase)] -> [String]
               -> ([(String, Maybe Phase)], [String])
partition_args [] srcs objs = (reverse srcs, reverse objs)
partition_args ("-x":suff:args) srcs objs
  | "none" <- suff      = partition_args args srcs objs
  | StopLn <- phase     = partition_args args srcs (slurp ++ objs)
  | otherwise           = partition_args rest (these_srcs ++ srcs) objs
        where phase = startPhase suff
              (slurp,rest) = break (== "-x") args
              these_srcs = zip slurp (repeat (Just phase))
partition_args (arg:args) srcs objs
  | looks_like_an_input arg = partition_args args ((arg,Nothing):srcs) objs
  | otherwise               = partition_args args srcs (arg:objs)


looks_like_an_input :: String -> Bool
looks_like_an_input m =  isSourceFilename m
                      || looksLikeModuleName m
                      || "-" `isPrefixOf` m
                      || not (hasExtension m)


-- | To simplify the handling of filepaths, we normalise all filepaths right
-- away. Note the asymmetry of FilePath.normalise:
--    Linux:   p\/q -> p\/q; p\\q -> p\\q
--    Windows: p\/q -> p\\q; p\\q -> p\\q
-- #12674: Filenames starting with a hyphen get normalised from ./-foo.hs
-- to -foo.hs. We have to re-prepend the current directory.
normalise_hyp :: FilePath -> FilePath
normalise_hyp fp
  | strt_dot_sl && "-" `isPrefixOf` nfp = cur_dir ++ nfp
  | otherwise                           = nfp
  where
#if defined(mingw32_HOST_OS)
    strt_dot_sl = "./" `isPrefixOf` fp || ".\\" `isPrefixOf` fp
#else
    strt_dot_sl = "./" `isPrefixOf` fp
#endif
    cur_dir = '.' : [pathSeparator]
    nfp = normalise fp

-----------------------------------------------------------------------------

-- | Checks the set of new DynFlags for possibly erroneous option
-- combinations when invoking 'setSessionDynFlags' and friends, and if
-- found, returns a fixed copy (if possible).
checkNewDynFlags :: (MonadIO m) => Logger -> DynFlags -> m DynFlags
checkNewDynFlags logger dflags = do
  -- See Note [DynFlags consistency]
  let (dflags', warnings) = makeDynFlagsConsistent dflags
  let diag_opts = initDiagOpts dflags
  liftIO $ handleFlagWarnings logger diag_opts (map (Warn WarningWithoutFlag) warnings)
  return dflags'

checkNewInteractiveDynFlags :: (MonadIO m) => Logger -> DynFlags -> m DynFlags
checkNewInteractiveDynFlags logger dflags0 = do
  -- We currently don't support use of StaticPointers in expressions entered on
  -- the REPL. See #12356.
  if xopt LangExt.StaticPointers dflags0
  then do
    let diag_opts = initDiagOpts dflags0
    liftIO $ printOrThrowDiagnostics logger diag_opts $ singleMessage
      $ fmap GhcDriverMessage
      $ mkPlainMsgEnvelope diag_opts interactiveSrcSpan DriverStaticPointersNotSupported
    return $ xopt_unset dflags0 LangExt.StaticPointers
  else return dflags0


-- %************************************************************************
-- %*                                                                      *
--             Setting, getting, and modifying the targets
-- %*                                                                      *
-- %************************************************************************

-- ToDo: think about relative vs. absolute file paths. And what
-- happens when the current directory changes.

-- | Sets the targets for this session.  Each target may be a module name
-- or a filename.  The targets correspond to the set of root modules for
-- the program\/library.  Unloading the current program is achieved by
-- setting the current set of targets to be empty, followed by 'load'.
setTargets :: GhcMonad m => [Target] -> m ()
setTargets targets = modifySession (\h -> h{ hsc_targets = targets })

-- | Returns the current set of targets
getTargets :: GhcMonad m => m [Target]
getTargets = withSession (return . hsc_targets)

-- | Add another target.
addTarget :: GhcMonad m => Target -> m ()
addTarget target
  = modifySession (\h -> h{ hsc_targets = target : hsc_targets h })

-- | Remove a target
removeTarget :: GhcMonad m => TargetId -> m ()
removeTarget target_id
  = modifySession (\h -> h{ hsc_targets = filter (hsc_targets h) })
  where
   filter targets = [ t | t@Target { targetId = id } <- targets, id /= target_id ]

-- | Attempts to guess what Target a string refers to.  This function
-- implements the @--make@/GHCi command-line syntax for filenames:
--
--   - if the string looks like a Haskell source filename, then interpret it
--     as such
--
--   - if adding a .hs or .lhs suffix yields the name of an existing file,
--     then use that
--
--   - otherwise interpret the string as a module name
--
guessTarget :: GhcMonad m => String -> Maybe UnitId -> Maybe Phase -> m Target
guessTarget str mUnitId (Just phase)
   = do
     tuid <- unitIdOrHomeUnit mUnitId
     return (Target (TargetFile str (Just phase)) True tuid Nothing)
guessTarget str mUnitId Nothing
   | isHaskellSrcFilename file
   = target (TargetFile file Nothing)
   | otherwise
   = do exists <- liftIO $ doesFileExist hs_file
        if exists
           then target (TargetFile hs_file Nothing)
           else do
        exists <- liftIO $ doesFileExist lhs_file
        if exists
           then target (TargetFile lhs_file Nothing)
           else do
        if looksLikeModuleName file
           then target (TargetModule (mkModuleName file))
           else do
        dflags <- getDynFlags
        liftIO $ throwGhcExceptionIO
                 (ProgramError (showSDoc dflags $
                 text "target" <+> quotes (text file) <+>
                 text "is not a module name or a source file"))
     where
         (file,obj_allowed)
                | '*':rest <- str = (rest, False)
                | otherwise       = (str,  True)

         hs_file  = file <.> "hs"
         lhs_file = file <.> "lhs"

         target tid = do
           tuid <- unitIdOrHomeUnit mUnitId
           pure $ Target tid obj_allowed tuid Nothing

-- | Unwrap 'UnitId' or retrieve the 'UnitId'
-- of the current 'HomeUnit'.
unitIdOrHomeUnit :: GhcMonad m => Maybe UnitId -> m UnitId
unitIdOrHomeUnit mUnitId = do
  currentHomeUnitId <- homeUnitId . hsc_home_unit <$> getSession
  pure (fromMaybe currentHomeUnitId mUnitId)

-- | Inform GHC that the working directory has changed.  GHC will flush
-- its cache of module locations, since it may no longer be valid.
--
-- Note: Before changing the working directory make sure all threads running
-- in the same session have stopped.  If you change the working directory,
-- you should also unload the current program (set targets to empty,
-- followed by load).
workingDirectoryChanged :: GhcMonad m => m ()
workingDirectoryChanged = do
  hsc_env <- getSession
  liftIO $ flushFinderCaches (hsc_FC hsc_env) (hsc_home_unit hsc_env)


-- %************************************************************************
-- %*                                                                      *
--             Running phases one at a time
-- %*                                                                      *
-- %************************************************************************

class ParsedMod m where
  modSummary   :: m -> ModSummary
  parsedSource :: m -> ParsedSource

class ParsedMod m => TypecheckedMod m where
  renamedSource     :: m -> Maybe RenamedSource
  typecheckedSource :: m -> TypecheckedSource
  moduleInfo        :: m -> ModuleInfo
  tm_internals      :: m -> (TcGblEnv, ModDetails)
        -- ToDo: improvements that could be made here:
        --  if the module succeeded renaming but not typechecking,
        --  we can still get back the GlobalRdrEnv and exports, so
        --  perhaps the ModuleInfo should be split up into separate
        --  fields.

class TypecheckedMod m => DesugaredMod m where
  coreModule :: m -> ModGuts

-- | The result of successful parsing.
data ParsedModule =
  ParsedModule { pm_mod_summary   :: ModSummary
               , pm_parsed_source :: ParsedSource
               , pm_extra_src_files :: [FilePath] }

instance ParsedMod ParsedModule where
  modSummary m    = pm_mod_summary m
  parsedSource m = pm_parsed_source m

-- | The result of successful typechecking.  It also contains the parser
--   result.
data TypecheckedModule =
  TypecheckedModule { tm_parsed_module       :: ParsedModule
                    , tm_renamed_source      :: Maybe RenamedSource
                    , tm_typechecked_source  :: TypecheckedSource
                    , tm_checked_module_info :: ModuleInfo
                    , tm_internals_          :: (TcGblEnv, ModDetails)
                    }

instance ParsedMod TypecheckedModule where
  modSummary m   = modSummary (tm_parsed_module m)
  parsedSource m = parsedSource (tm_parsed_module m)

instance TypecheckedMod TypecheckedModule where
  renamedSource m     = tm_renamed_source m
  typecheckedSource m = tm_typechecked_source m
  moduleInfo m        = tm_checked_module_info m
  tm_internals m      = tm_internals_ m

-- | The result of successful desugaring (i.e., translation to core).  Also
--  contains all the information of a typechecked module.
data DesugaredModule =
  DesugaredModule { dm_typechecked_module :: TypecheckedModule
                  , dm_core_module        :: ModGuts
             }

instance ParsedMod DesugaredModule where
  modSummary m   = modSummary (dm_typechecked_module m)
  parsedSource m = parsedSource (dm_typechecked_module m)

instance TypecheckedMod DesugaredModule where
  renamedSource m     = renamedSource (dm_typechecked_module m)
  typecheckedSource m = typecheckedSource (dm_typechecked_module m)
  moduleInfo m        = moduleInfo (dm_typechecked_module m)
  tm_internals m      = tm_internals_ (dm_typechecked_module m)

instance DesugaredMod DesugaredModule where
  coreModule m = dm_core_module m

type ParsedSource      = Located HsModule
type RenamedSource     = (HsGroup GhcRn, [LImportDecl GhcRn], Maybe [(LIE GhcRn, Avails)],
                          Maybe LHsDocString)
type TypecheckedSource = LHsBinds GhcTc

-- NOTE:
--   - things that aren't in the output of the typechecker right now:
--     - the export list
--     - the imports
--     - type signatures
--     - type/data/newtype declarations
--     - class declarations
--     - instances
--   - extra things in the typechecker's output:
--     - default methods are turned into top-level decls.
--     - dictionary bindings

-- | Return the 'ModSummary' of a module with the given name.
--
-- The module must be part of the module graph (see 'hsc_mod_graph' and
-- 'ModuleGraph').  If this is not the case, this function will throw a
-- 'GhcApiError'.
--
-- This function ignores boot modules and requires that there is only one
-- non-boot module with the given name.
getModSummary :: GhcMonad m => ModuleName -> m ModSummary
getModSummary mod = do
   mg <- liftM hsc_mod_graph getSession
   let mods_by_name = [ ms | ms <- mgModSummaries mg
                      , ms_mod_name ms == mod
                      , isBootSummary ms == NotBoot ]
   case mods_by_name of
     [] -> do dflags <- getDynFlags
              liftIO $ throwIO $ mkApiErr dflags (text "Module not part of module graph")
     [ms] -> return ms
     multiple -> do dflags <- getDynFlags
                    liftIO $ throwIO $ mkApiErr dflags (text "getModSummary is ambiguous: " <+> ppr multiple)

-- | Parse a module.
--
-- Throws a 'SourceError' on parse error.
parseModule :: GhcMonad m => ModSummary -> m ParsedModule
parseModule ms = do
   hsc_env <- getSession
   liftIO $ do
     let lcl_hsc_env = hscSetFlags (ms_hspp_opts ms) hsc_env
     hpm <- hscParse lcl_hsc_env ms
     return (ParsedModule ms (hpm_module hpm) (hpm_src_files hpm))
               -- See Note [exact print annotations] in GHC.Parser.Annotation

-- | Typecheck and rename a parsed module.
--
-- Throws a 'SourceError' if either fails.
typecheckModule :: GhcMonad m => ParsedModule -> m TypecheckedModule
typecheckModule pmod = do
 hsc_env <- getSession

 liftIO $ do
   let ms          = modSummary pmod
   let lcl_dflags  = ms_hspp_opts ms -- take into account pragmas (OPTIONS_GHC, etc.)
   let lcl_hsc_env = hscSetFlags lcl_dflags hsc_env
   let lcl_logger  = hsc_logger lcl_hsc_env
   (tc_gbl_env, rn_info) <- hscTypecheckRename lcl_hsc_env ms $
                        HsParsedModule { hpm_module = parsedSource pmod,
                                         hpm_src_files = pm_extra_src_files pmod }
   details <- makeSimpleDetails lcl_logger tc_gbl_env
   safe    <- finalSafeMode lcl_dflags tc_gbl_env

   return $
     TypecheckedModule {
       tm_internals_          = (tc_gbl_env, details),
       tm_parsed_module       = pmod,
       tm_renamed_source      = rn_info,
       tm_typechecked_source  = tcg_binds tc_gbl_env,
       tm_checked_module_info =
         ModuleInfo {
           minf_type_env  = md_types details,
           minf_exports   = md_exports details,
           minf_rdr_env   = Just (tcg_rdr_env tc_gbl_env),
           minf_instances = fixSafeInstances safe $ md_insts details,
           minf_iface     = Nothing,
           minf_safe      = safe,
           minf_modBreaks = emptyModBreaks
         }}

-- | Desugar a typechecked module.
desugarModule :: GhcMonad m => TypecheckedModule -> m DesugaredModule
desugarModule tcm = do
 hsc_env <- getSession
 liftIO $ do
   let ms = modSummary tcm
   let (tcg, _) = tm_internals tcm
   let lcl_hsc_env = hscSetFlags (ms_hspp_opts ms) hsc_env
   guts <- hscDesugar lcl_hsc_env ms tcg
   return $
     DesugaredModule {
       dm_typechecked_module = tcm,
       dm_core_module        = guts
     }



-- %************************************************************************
-- %*                                                                      *
--             Dealing with Core
-- %*                                                                      *
-- %************************************************************************

-- | A CoreModule consists of just the fields of a 'ModGuts' that are needed for
-- the 'GHC.compileToCoreModule' interface.
data CoreModule
  = CoreModule {
      -- | Module name
      cm_module   :: !Module,
      -- | Type environment for types declared in this module
      cm_types    :: !TypeEnv,
      -- | Declarations
      cm_binds    :: CoreProgram,
      -- | Safe Haskell mode
      cm_safe     :: SafeHaskellMode
    }

instance Outputable CoreModule where
   ppr (CoreModule {cm_module = mn, cm_types = te, cm_binds = cb,
                    cm_safe = sf})
    = text "%module" <+> ppr mn <+> parens (ppr sf) <+> ppr te
      $$ vcat (map ppr cb)

-- | This is the way to get access to the Core bindings corresponding
-- to a module. 'compileToCore' parses, typechecks, and
-- desugars the module, then returns the resulting Core module (consisting of
-- the module name, type declarations, and function declarations) if
-- successful.
compileToCoreModule :: GhcMonad m => FilePath -> m CoreModule
compileToCoreModule = compileCore False

-- | Like compileToCoreModule, but invokes the simplifier, so
-- as to return simplified and tidied Core.
compileToCoreSimplified :: GhcMonad m => FilePath -> m CoreModule
compileToCoreSimplified = compileCore True

compileCore :: GhcMonad m => Bool -> FilePath -> m CoreModule
compileCore simplify fn = do
   -- First, set the target to the desired filename
   target <- guessTarget fn Nothing Nothing
   addTarget target
   _ <- load LoadAllTargets
   -- Then find dependencies
   modGraph <- depanal [] True
   case find ((== fn) . msHsFilePath) (mgModSummaries modGraph) of
     Just modSummary -> do
       -- Now we have the module name;
       -- parse, typecheck and desugar the module
       (tcg, mod_guts) <- -- TODO: space leaky: call hsc* directly?
         do tm <- typecheckModule =<< parseModule modSummary
            let tcg = fst (tm_internals tm)
            (,) tcg . coreModule <$> desugarModule tm
       liftM (gutsToCoreModule (mg_safe_haskell mod_guts)) $
         if simplify
          then do
             -- If simplify is true: simplify (hscSimplify), then tidy
             -- (tidyProgram).
             hsc_env <- getSession
             simpl_guts <- liftIO $ do
               plugins <- readIORef (tcg_th_coreplugins tcg)
               hscSimplify hsc_env plugins mod_guts
             tidy_guts <- liftIO $ tidyProgram hsc_env simpl_guts
             return $ Left tidy_guts
          else
             return $ Right mod_guts

     Nothing -> panic "compileToCoreModule: target FilePath not found in\
                           module dependency graph"
  where -- two versions, based on whether we simplify (thus run tidyProgram,
        -- which returns a (CgGuts, ModDetails) pair, or not (in which case
        -- we just have a ModGuts.
        gutsToCoreModule :: SafeHaskellMode
                         -> Either (CgGuts, ModDetails) ModGuts
                         -> CoreModule
        gutsToCoreModule safe_mode (Left (cg, md)) = CoreModule {
          cm_module = cg_module cg,
          cm_types  = md_types md,
          cm_binds  = cg_binds cg,
          cm_safe   = safe_mode
        }
        gutsToCoreModule safe_mode (Right mg) = CoreModule {
          cm_module  = mg_module mg,
          cm_types   = typeEnvFromEntities (bindersOfBinds (mg_binds mg))
                                           (mg_tcs mg) (mg_patsyns mg)
                                           (mg_fam_insts mg),
          cm_binds   = mg_binds mg,
          cm_safe    = safe_mode
         }

-- %************************************************************************
-- %*                                                                      *
--             Inspecting the session
-- %*                                                                      *
-- %************************************************************************

-- | Get the module dependency graph.
getModuleGraph :: GhcMonad m => m ModuleGraph -- ToDo: DiGraph ModSummary
getModuleGraph = liftM hsc_mod_graph getSession

-- | Return @True@ \<==> module is loaded.
isLoaded :: GhcMonad m => ModuleName -> m Bool
isLoaded m = withSession $ \hsc_env ->
  return $! isJust (lookupHpt (hsc_HPT hsc_env) m)

-- | Return the bindings for the current interactive session.
getBindings :: GhcMonad m => m [TyThing]
getBindings = withSession $ \hsc_env ->
    return $ icInScopeTTs $ hsc_IC hsc_env

-- | Return the instances for the current interactive session.
getInsts :: GhcMonad m => m ([ClsInst], [FamInst])
getInsts = withSession $ \hsc_env ->
    return $ ic_instances (hsc_IC hsc_env)

getPrintUnqual :: GhcMonad m => m PrintUnqualified
getPrintUnqual = withSession $ \hsc_env -> do
  return $ icPrintUnqual (hsc_unit_env hsc_env) (hsc_IC hsc_env)

-- | Container for information about a 'Module'.
data ModuleInfo = ModuleInfo {
        minf_type_env  :: TypeEnv,
        minf_exports   :: [AvailInfo],
        minf_rdr_env   :: Maybe GlobalRdrEnv,   -- Nothing for a compiled/package mod
        minf_instances :: [ClsInst],
        minf_iface     :: Maybe ModIface,
        minf_safe      :: SafeHaskellMode,
        minf_modBreaks :: ModBreaks
  }
        -- We don't want HomeModInfo here, because a ModuleInfo applies
        -- to package modules too.

-- | Request information about a loaded 'Module'
getModuleInfo :: GhcMonad m => Module -> m (Maybe ModuleInfo)  -- XXX: Maybe X
getModuleInfo mdl = withSession $ \hsc_env -> do
  let mg = hsc_mod_graph hsc_env
  if mgElemModule mg mdl
        then liftIO $ getHomeModuleInfo hsc_env mdl
        else do
  {- if isHomeModule (hsc_dflags hsc_env) mdl
        then return Nothing
        else -} liftIO $ getPackageModuleInfo hsc_env mdl
   -- ToDo: we don't understand what the following comment means.
   --    (SDM, 19/7/2011)
   -- getPackageModuleInfo will attempt to find the interface, so
   -- we don't want to call it for a home module, just in case there
   -- was a problem loading the module and the interface doesn't
   -- exist... hence the isHomeModule test here.  (ToDo: reinstate)

getPackageModuleInfo :: HscEnv -> Module -> IO (Maybe ModuleInfo)
getPackageModuleInfo hsc_env mdl
  = do  eps <- hscEPS hsc_env
        iface <- hscGetModuleInterface hsc_env mdl
        let
            avails = mi_exports iface
            pte    = eps_PTE eps
            tys    = [ ty | name <- concatMap availNames avails,
                            Just ty <- [lookupTypeEnv pte name] ]
        --
        return (Just (ModuleInfo {
                        minf_type_env  = mkTypeEnv tys,
                        minf_exports   = avails,
                        minf_rdr_env   = Just $! availsToGlobalRdrEnv (moduleName mdl) avails,
                        minf_instances = error "getModuleInfo: instances for package module unimplemented",
                        minf_iface     = Just iface,
                        minf_safe      = getSafeMode $ mi_trust iface,
                        minf_modBreaks = emptyModBreaks
                }))

availsToGlobalRdrEnv :: ModuleName -> [AvailInfo] -> GlobalRdrEnv
availsToGlobalRdrEnv mod_name avails
  = mkGlobalRdrEnv (gresFromAvails (Just imp_spec) avails)
  where
      -- We're building a GlobalRdrEnv as if the user imported
      -- all the specified modules into the global interactive module
    imp_spec = ImpSpec { is_decl = decl, is_item = ImpAll}
    decl = ImpDeclSpec { is_mod = mod_name, is_as = mod_name,
                         is_qual = False,
                         is_dloc = srcLocSpan interactiveSrcLoc }


getHomeModuleInfo :: HscEnv -> Module -> IO (Maybe ModuleInfo)
getHomeModuleInfo hsc_env mdl =
  case lookupHpt (hsc_HPT hsc_env) (moduleName mdl) of
    Nothing  -> return Nothing
    Just hmi -> do
      let details = hm_details hmi
          iface   = hm_iface hmi
      return (Just (ModuleInfo {
                        minf_type_env  = md_types details,
                        minf_exports   = md_exports details,
                        minf_rdr_env   = mi_globals $! hm_iface hmi,
                        minf_instances = md_insts details,
                        minf_iface     = Just iface,
                        minf_safe      = getSafeMode $ mi_trust iface
                       ,minf_modBreaks = getModBreaks hmi
                        }))

-- | The list of top-level entities defined in a module
modInfoTyThings :: ModuleInfo -> [TyThing]
modInfoTyThings minf = typeEnvElts (minf_type_env minf)

modInfoTopLevelScope :: ModuleInfo -> Maybe [Name]
modInfoTopLevelScope minf
  = fmap (map greMangledName . globalRdrEnvElts) (minf_rdr_env minf)

modInfoExports :: ModuleInfo -> [Name]
modInfoExports minf = concatMap availNames $! minf_exports minf

modInfoExportsWithSelectors :: ModuleInfo -> [Name]
modInfoExportsWithSelectors minf = concatMap availNamesWithSelectors $! minf_exports minf

-- | Returns the instances defined by the specified module.
-- Warning: currently unimplemented for package modules.
modInfoInstances :: ModuleInfo -> [ClsInst]
modInfoInstances = minf_instances

modInfoIsExportedName :: ModuleInfo -> Name -> Bool
modInfoIsExportedName minf name = elemNameSet name (availsToNameSet (minf_exports minf))

mkPrintUnqualifiedForModule :: GhcMonad m =>
                               ModuleInfo
                            -> m (Maybe PrintUnqualified) -- XXX: returns a Maybe X
mkPrintUnqualifiedForModule minf = withSession $ \hsc_env -> do
  let mk_print_unqual = mkPrintUnqualified (hsc_unit_env hsc_env)
  return (fmap mk_print_unqual (minf_rdr_env minf))

modInfoLookupName :: GhcMonad m =>
                     ModuleInfo -> Name
                  -> m (Maybe TyThing) -- XXX: returns a Maybe X
modInfoLookupName minf name = withSession $ \hsc_env -> do
   case lookupTypeEnv (minf_type_env minf) name of
     Just tyThing -> return (Just tyThing)
     Nothing      -> liftIO (lookupType hsc_env name)

modInfoIface :: ModuleInfo -> Maybe ModIface
modInfoIface = minf_iface

modInfoRdrEnv :: ModuleInfo -> Maybe GlobalRdrEnv
modInfoRdrEnv = minf_rdr_env

-- | Retrieve module safe haskell mode
modInfoSafe :: ModuleInfo -> SafeHaskellMode
modInfoSafe = minf_safe

modInfoModBreaks :: ModuleInfo -> ModBreaks
modInfoModBreaks = minf_modBreaks

isDictonaryId :: Id -> Bool
isDictonaryId id
  = case tcSplitSigmaTy (idType id) of {
      (_tvs, _theta, tau) -> isDictTy tau }

-- | Looks up a global name: that is, any top-level name in any
-- visible module.  Unlike 'lookupName', lookupGlobalName does not use
-- the interactive context, and therefore does not require a preceding
-- 'setContext'.
lookupGlobalName :: GhcMonad m => Name -> m (Maybe TyThing)
lookupGlobalName name = withSession $ \hsc_env -> do
   liftIO $ lookupType hsc_env name

findGlobalAnns :: (GhcMonad m, Typeable a) => ([Word8] -> a) -> AnnTarget Name -> m [a]
findGlobalAnns deserialize target = withSession $ \hsc_env -> do
    ann_env <- liftIO $ prepareAnnotations hsc_env Nothing
    return (findAnns deserialize ann_env target)

-- | get the GlobalRdrEnv for a session
getGRE :: GhcMonad m => m GlobalRdrEnv
getGRE = withSession $ \hsc_env-> return $ icReaderEnv (hsc_IC hsc_env)

-- | Retrieve all type and family instances in the environment, indexed
-- by 'Name'. Each name's lists will contain every instance in which that name
-- is mentioned in the instance head.
getNameToInstancesIndex :: GhcMonad m
  => [Module]        -- ^ visible modules. An orphan instance will be returned
                     -- if it is visible from at least one module in the list.
  -> Maybe [Module]  -- ^ modules to load. If this is not specified, we load
                     -- modules for everything that is in scope unqualified.
  -> m (Messages TcRnMessage, Maybe (NameEnv ([ClsInst], [FamInst])))
getNameToInstancesIndex visible_mods mods_to_load = do
  hsc_env <- getSession
  liftIO $ runTcInteractive hsc_env $
    do { case mods_to_load of
           Nothing -> loadUnqualIfaces hsc_env (hsc_IC hsc_env)
           Just mods ->
             let doc = text "Need interface for reporting instances in scope"
             in initIfaceTcRn $ mapM_ (loadSysInterface doc) mods

       ; InstEnvs {ie_global, ie_local} <- tcGetInstEnvs
       ; let visible_mods' = mkModuleSet visible_mods
       ; (pkg_fie, home_fie) <- tcGetFamInstEnvs
       -- We use Data.Sequence.Seq because we are creating left associated
       -- mappends.
       -- cls_index and fam_index below are adapted from GHC.Tc.Module.lookupInsts
       ; let cls_index = Map.fromListWith mappend
                 [ (n, Seq.singleton ispec)
                 | ispec <- instEnvElts ie_local ++ instEnvElts ie_global
                 , instIsVisible visible_mods' ispec
                 , n <- nameSetElemsStable $ orphNamesOfClsInst ispec
                 ]
       ; let fam_index = Map.fromListWith mappend
                 [ (n, Seq.singleton fispec)
                 | fispec <- famInstEnvElts home_fie ++ famInstEnvElts pkg_fie
                 , n <- nameSetElemsStable $ orphNamesOfFamInst fispec
                 ]
       ; return $ mkNameEnv $
           [ (nm, (toList clss, toList fams))
           | (nm, (clss, fams)) <- Map.toList $ Map.unionWith mappend
               (fmap (,Seq.empty) cls_index)
               (fmap (Seq.empty,) fam_index)
           ] }

-- -----------------------------------------------------------------------------
-- Misc exported utils

dataConType :: DataCon -> Type
dataConType dc = idType (dataConWrapId dc)

-- | print a 'NamedThing', adding parentheses if the name is an operator.
pprParenSymName :: NamedThing a => a -> SDoc
pprParenSymName a = parenSymOcc (getOccName a) (ppr (getName a))

-- ----------------------------------------------------------------------------


-- ToDo:
--   - Data and Typeable instances for HsSyn.

-- ToDo: check for small transformations that happen to the syntax in
-- the typechecker (eg. -e ==> negate e, perhaps for fromIntegral)

-- ToDo: maybe use TH syntax instead of Iface syntax?  There's already a way
-- to get from TyCons, Ids etc. to TH syntax (reify).

-- :browse will use either lm_toplev or inspect lm_interface, depending
-- on whether the module is interpreted or not.


-- Extract the filename, stringbuffer content and dynflags associed to a ModSummary
-- Given an initialised GHC session a ModSummary can be retrieved for
-- a module by using 'getModSummary'
--
-- XXX: Explain pre-conditions
getModuleSourceAndFlags :: ModSummary -> IO (String, StringBuffer, DynFlags)
getModuleSourceAndFlags m = do
  case ml_hs_file $ ms_location m of
    Nothing -> throwIO $ mkApiErr (ms_hspp_opts m) (text "No source available for module " <+> ppr (ms_mod m))
    Just sourceFile -> do
        source <- hGetStringBuffer sourceFile
        return (sourceFile, source, ms_hspp_opts m)


-- | Return module source as token stream, including comments.
--
-- A 'Module' can be turned into a 'ModSummary' using 'getModSummary' if
-- your session is fully initialised.
-- Throws a 'GHC.Driver.Env.SourceError' on parse error.
getTokenStream :: ModSummary -> IO [Located Token]
getTokenStream mod = do
  (sourceFile, source, dflags) <- getModuleSourceAndFlags mod
  let startLoc = mkRealSrcLoc (mkFastString sourceFile) 1 1
  case lexTokenStream (initParserOpts dflags) source startLoc of
    POk _ ts    -> return ts
    PFailed pst -> throwErrors (GhcPsMessage <$> getPsErrorMessages pst)

-- | Give even more information on the source than 'getTokenStream'
-- This function allows reconstructing the source completely with
-- 'showRichTokenStream'.
getRichTokenStream :: ModSummary -> IO [(Located Token, String)]
getRichTokenStream mod = do
  (sourceFile, source, dflags) <- getModuleSourceAndFlags mod
  let startLoc = mkRealSrcLoc (mkFastString sourceFile) 1 1
  case lexTokenStream (initParserOpts dflags) source startLoc of
    POk _ ts    -> return $ addSourceToTokens startLoc source ts
    PFailed pst -> throwErrors (GhcPsMessage <$> getPsErrorMessages pst)

-- | Given a source location and a StringBuffer corresponding to this
-- location, return a rich token stream with the source associated to the
-- tokens.
addSourceToTokens :: RealSrcLoc -> StringBuffer -> [Located Token]
                  -> [(Located Token, String)]
addSourceToTokens _ _ [] = []
addSourceToTokens loc buf (t@(L span _) : ts)
    = case span of
      UnhelpfulSpan _ -> (t,"") : addSourceToTokens loc buf ts
      RealSrcSpan s _ -> (t,str) : addSourceToTokens newLoc newBuf ts
        where
          (newLoc, newBuf, str) = go "" loc buf
          start = realSrcSpanStart s
          end = realSrcSpanEnd s
          go acc loc buf | loc < start = go acc nLoc nBuf
                         | start <= loc && loc < end = go (ch:acc) nLoc nBuf
                         | otherwise = (loc, buf, reverse acc)
              where (ch, nBuf) = nextChar buf
                    nLoc = advanceSrcLoc loc ch


-- | Take a rich token stream such as produced from 'getRichTokenStream' and
-- return source code almost identical to the original code (except for
-- insignificant whitespace.)
showRichTokenStream :: [(Located Token, String)] -> String
showRichTokenStream ts = go startLoc ts ""
    where sourceFile = getFile $ map (getLoc . fst) ts
          getFile [] = panic "showRichTokenStream: No source file found"
          getFile (UnhelpfulSpan _ : xs) = getFile xs
          getFile (RealSrcSpan s _ : _) = srcSpanFile s
          startLoc = mkRealSrcLoc sourceFile 1 1
          go _ [] = id
          go loc ((L span _, str):ts)
              = case span of
                UnhelpfulSpan _ -> go loc ts
                RealSrcSpan s _
                 | locLine == tokLine -> ((replicate (tokCol - locCol) ' ') ++)
                                       . (str ++)
                                       . go tokEnd ts
                 | otherwise -> ((replicate (tokLine - locLine) '\n') ++)
                               . ((replicate (tokCol - 1) ' ') ++)
                              . (str ++)
                              . go tokEnd ts
                  where (locLine, locCol) = (srcLocLine loc, srcLocCol loc)
                        (tokLine, tokCol) = (srcSpanStartLine s, srcSpanStartCol s)
                        tokEnd = realSrcSpanEnd s

-- -----------------------------------------------------------------------------
-- Interactive evaluation

-- | Takes a 'ModuleName' and possibly a 'UnitId', and consults the
-- filesystem and package database to find the corresponding 'Module',
-- using the algorithm that is used for an @import@ declaration.
findModule :: GhcMonad m => ModuleName -> Maybe FastString -> m Module
findModule mod_name maybe_pkg = do
  pkg_qual <- renamePkgQualM maybe_pkg
  findQualifiedModule pkg_qual mod_name


findQualifiedModule :: GhcMonad m => PkgQual -> ModuleName -> m Module
findQualifiedModule pkgqual mod_name = withSession $ \hsc_env -> do
  let fc        = hsc_FC hsc_env
  let home_unit = hsc_home_unit hsc_env
  let units     = hsc_units hsc_env
  let dflags    = hsc_dflags hsc_env
  let fopts     = initFinderOpts dflags
  case pkgqual of
    ThisPkg _ -> do
      home <- lookupLoadedHomeModule mod_name
      case home of
        Just m  -> return m
        Nothing -> liftIO $ do
           res <- findImportedModule fc fopts units home_unit mod_name pkgqual
           case res of
             Found loc m | not (isHomeModule home_unit m) -> return m
                         | otherwise -> modNotLoadedError dflags m loc
             err -> throwOneError $ noModError hsc_env noSrcSpan mod_name err

    _ -> liftIO $ do
      res <- findImportedModule fc fopts units home_unit mod_name pkgqual
      case res of
        Found _ m -> return m
        err       -> throwOneError $ noModError hsc_env noSrcSpan mod_name err


modNotLoadedError :: DynFlags -> Module -> ModLocation -> IO a
modNotLoadedError dflags m loc = throwGhcExceptionIO $ CmdLineError $ showSDoc dflags $
   text "module is not loaded:" <+>
   quotes (ppr (moduleName m)) <+>
   parens (text (expectJust "modNotLoadedError" (ml_hs_file loc)))

renamePkgQualM :: GhcMonad m => Maybe FastString -> m PkgQual
renamePkgQualM p = withSession $ \hsc_env -> pure (renamePkgQual (hsc_unit_env hsc_env) p)

renameRawPkgQualM :: GhcMonad m => RawPkgQual -> m PkgQual
renameRawPkgQualM p = withSession $ \hsc_env -> pure (renameRawPkgQual (hsc_unit_env hsc_env) p)

-- | Like 'findModule', but differs slightly when the module refers to
-- a source file, and the file has not been loaded via 'load'.  In
-- this case, 'findModule' will throw an error (module not loaded),
-- but 'lookupModule' will check to see whether the module can also be
-- found in a package, and if so, that package 'Module' will be
-- returned.  If not, the usual module-not-found error will be thrown.
--
lookupModule :: GhcMonad m => ModuleName -> Maybe FastString -> m Module
lookupModule mod_name maybe_pkg = do
  pkgqual <- renamePkgQualM maybe_pkg
  lookupQualifiedModule pkgqual mod_name

lookupQualifiedModule :: GhcMonad m => PkgQual -> ModuleName -> m Module
lookupQualifiedModule NoPkgQual mod_name = withSession $ \hsc_env -> do
  home <- lookupLoadedHomeModule mod_name
  case home of
    Just m  -> return m
    Nothing -> liftIO $ do
      let fc     = hsc_FC hsc_env
      let units  = hsc_units hsc_env
      let dflags = hsc_dflags hsc_env
      let fopts  = initFinderOpts dflags
      res <- findExposedPackageModule fc fopts units mod_name NoPkgQual
      case res of
        Found _ m -> return m
        err       -> throwOneError $ noModError hsc_env noSrcSpan mod_name err
lookupQualifiedModule pkgqual mod_name = findQualifiedModule pkgqual mod_name

lookupLoadedHomeModule :: GhcMonad m => ModuleName -> m (Maybe Module)
lookupLoadedHomeModule mod_name = withSession $ \hsc_env ->
  case lookupHpt (hsc_HPT hsc_env) mod_name of
    Just mod_info      -> return (Just (mi_module (hm_iface mod_info)))
    _not_a_home_module -> return Nothing

-- | Check that a module is safe to import (according to Safe Haskell).
--
-- We return True to indicate the import is safe and False otherwise
-- although in the False case an error may be thrown first.
isModuleTrusted :: GhcMonad m => Module -> m Bool
isModuleTrusted m = withSession $ \hsc_env ->
    liftIO $ hscCheckSafe hsc_env m noSrcSpan

-- | Return if a module is trusted and the pkgs it depends on to be trusted.
moduleTrustReqs :: GhcMonad m => Module -> m (Bool, Set UnitId)
moduleTrustReqs m = withSession $ \hsc_env ->
    liftIO $ hscGetSafe hsc_env m noSrcSpan

-- | Set the monad GHCi lifts user statements into.
--
-- Checks that a type (in string form) is an instance of the
-- @GHC.GHCi.GHCiSandboxIO@ type class. Sets it to be the GHCi monad if it is,
-- throws an error otherwise.
setGHCiMonad :: GhcMonad m => String -> m ()
setGHCiMonad name = withSession $ \hsc_env -> do
    ty <- liftIO $ hscIsGHCiMonad hsc_env name
    modifySession $ \s ->
        let ic = (hsc_IC s) { ic_monad = ty }
        in s { hsc_IC = ic }

-- | Get the monad GHCi lifts user statements into.
getGHCiMonad :: GhcMonad m => m Name
getGHCiMonad = fmap (ic_monad . hsc_IC) getSession

getHistorySpan :: GhcMonad m => History -> m SrcSpan
getHistorySpan h = withSession $ \hsc_env ->
    return $ GHC.Runtime.Eval.getHistorySpan hsc_env h

obtainTermFromVal :: GhcMonad m => Int ->  Bool -> Type -> a -> m Term
obtainTermFromVal bound force ty a = withSession $ \hsc_env ->
    liftIO $ GHC.Runtime.Eval.obtainTermFromVal hsc_env bound force ty a

obtainTermFromId :: GhcMonad m => Int -> Bool -> Id -> m Term
obtainTermFromId bound force id = withSession $ \hsc_env ->
    liftIO $ GHC.Runtime.Eval.obtainTermFromId hsc_env bound force id


-- | Returns the 'TyThing' for a 'Name'.  The 'Name' may refer to any
-- entity known to GHC, including 'Name's defined using 'runStmt'.
lookupName :: GhcMonad m => Name -> m (Maybe TyThing)
lookupName name =
     withSession $ \hsc_env ->
       liftIO $ hscTcRcLookupName hsc_env name

-- -----------------------------------------------------------------------------
-- Pure API

-- | A pure interface to the module parser.
--
parser :: String         -- ^ Haskell module source text (full Unicode is supported)
       -> DynFlags       -- ^ the flags
       -> FilePath       -- ^ the filename (for source locations)
       -> (WarningMessages, Either ErrorMessages (Located HsModule))

parser str dflags filename =
   let
       loc  = mkRealSrcLoc (mkFastString filename) 1 1
       buf  = stringToStringBuffer str
   in
   case unP Parser.parseModule (initParserState (initParserOpts dflags) buf loc) of

     PFailed pst ->
         let (warns,errs) = getPsMessages pst in
         (GhcPsMessage <$> warns, Left $ GhcPsMessage <$> errs)

     POk pst rdr_module ->
         let (warns,_) = getPsMessages pst in
         (GhcPsMessage <$> warns, Right rdr_module)

-- -----------------------------------------------------------------------------
-- | Find the package environment (if one exists)
--
-- We interpret the package environment as a set of package flags; to be
-- specific, if we find a package environment file like
--
-- > clear-package-db
-- > global-package-db
-- > package-db blah/package.conf.d
-- > package-id id1
-- > package-id id2
--
-- we interpret this as
--
-- > [ -hide-all-packages
-- > , -clear-package-db
-- > , -global-package-db
-- > , -package-db blah/package.conf.d
-- > , -package-id id1
-- > , -package-id id2
-- > ]
--
-- There's also an older syntax alias for package-id, which is just an
-- unadorned package id
--
-- > id1
-- > id2
--
interpretPackageEnv :: Logger -> DynFlags -> IO DynFlags
interpretPackageEnv logger dflags = do
    mPkgEnv <- runMaybeT $ msum $ [
                   getCmdLineArg >>= \env -> msum [
                       probeNullEnv env
                     , probeEnvFile env
                     , probeEnvName env
                     , cmdLineError env
                     ]
                 , getEnvVar >>= \env -> msum [
                       probeNullEnv env
                     , probeEnvFile env
                     , probeEnvName env
                     , envError     env
                     ]
                 , notIfHideAllPackages >> msum [
                       findLocalEnvFile >>= probeEnvFile
                     , probeEnvName defaultEnvName
                     ]
                 ]
    case mPkgEnv of
      Nothing ->
        -- No environment found. Leave DynFlags unchanged.
        return dflags
      Just "-" -> do
        -- Explicitly disabled environment file. Leave DynFlags unchanged.
        return dflags
      Just envfile -> do
        content <- readFile envfile
        compilationProgressMsg logger (text "Loaded package environment from " <> text envfile)
        let (_, dflags') = runCmdLine (runEwM (setFlagsFromEnvFile envfile content)) dflags

        return dflags'
  where
    -- Loading environments (by name or by location)

    archOS = platformArchOS (targetPlatform dflags)

    namedEnvPath :: String -> MaybeT IO FilePath
    namedEnvPath name = do
     appdir <- versionedAppDir (programName dflags) archOS
     return $ appdir </> "environments" </> name

    probeEnvName :: String -> MaybeT IO FilePath
    probeEnvName name = probeEnvFile =<< namedEnvPath name

    probeEnvFile :: FilePath -> MaybeT IO FilePath
    probeEnvFile path = do
      guard =<< liftMaybeT (doesFileExist path)
      return path

    probeNullEnv :: FilePath -> MaybeT IO FilePath
    probeNullEnv "-" = return "-"
    probeNullEnv _   = mzero

    -- Various ways to define which environment to use

    getCmdLineArg :: MaybeT IO String
    getCmdLineArg = MaybeT $ return $ packageEnv dflags

    getEnvVar :: MaybeT IO String
    getEnvVar = do
      mvar <- liftMaybeT $ MC.try $ getEnv "GHC_ENVIRONMENT"
      case mvar of
        Right var -> return var
        Left err  -> if isDoesNotExistError err then mzero
                                                else liftMaybeT $ throwIO err

    notIfHideAllPackages :: MaybeT IO ()
    notIfHideAllPackages =
      guard (not (gopt Opt_HideAllPackages dflags))

    defaultEnvName :: String
    defaultEnvName = "default"

    -- e.g. .ghc.environment.x86_64-linux-7.6.3
    localEnvFileName :: FilePath
    localEnvFileName = ".ghc.environment" <.> versionedFilePath archOS

    -- Search for an env file, starting in the current dir and looking upwards.
    -- Fail if we get to the users home dir or the filesystem root. That is,
    -- we don't look for an env file in the user's home dir. The user-wide
    -- env lives in ghc's versionedAppDir/environments/default
    findLocalEnvFile :: MaybeT IO FilePath
    findLocalEnvFile = do
        curdir  <- liftMaybeT getCurrentDirectory
        homedir <- tryMaybeT getHomeDirectory
        let probe dir | isDrive dir || dir == homedir
                      = mzero
            probe dir = do
              let file = dir </> localEnvFileName
              exists <- liftMaybeT (doesFileExist file)
              if exists
                then return file
                else probe (takeDirectory dir)
        probe curdir

    -- Error reporting

    cmdLineError :: String -> MaybeT IO a
    cmdLineError env = liftMaybeT . throwGhcExceptionIO . CmdLineError $
      "Package environment " ++ show env ++ " not found"

    envError :: String -> MaybeT IO a
    envError env = liftMaybeT . throwGhcExceptionIO . CmdLineError $
         "Package environment "
      ++ show env
      ++ " (specified in GHC_ENVIRONMENT) not found"

-- | An error thrown if the GHC API is used in an incorrect fashion.
newtype GhcApiError = GhcApiError String

instance Show GhcApiError where
  show (GhcApiError msg) = msg

instance Exception GhcApiError

mkApiErr :: DynFlags -> SDoc -> GhcApiError
mkApiErr dflags msg = GhcApiError (showSDoc dflags msg)
