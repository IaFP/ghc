{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 903
{-# LANGUAGE QuantifiedConstraints, ExplicitNamespaces, TypeOperators #-}
#endif
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GHC.Settings.IO
 ( SettingsError (..)
 , initSettings
 ) where

import GHC.Prelude

import GHC.Settings.Utils

import GHC.Settings.Config
import GHC.Utils.CliOption
import GHC.Utils.Fingerprint
import GHC.Platform
import GHC.Utils.Panic
import GHC.Settings
import GHC.SysTools.BaseDir

import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import qualified Data.Map as Map
import System.FilePath
import System.Directory
#if MIN_VERSION_base(4,16,0)
import GHC.Types (Total)
#endif

data SettingsError
  = SettingsError_MissingData String
  | SettingsError_BadData String

initSettings
  :: forall m
  .  (
#if MIN_VERSION_base(4,16,0)
  Total m,
#endif
  MonadIO m)
  => String -- ^ TopDir path
  -> ExceptT SettingsError m Settings
initSettings top_dir = do
  -- see Note [topdir: How GHC finds its files]
  -- NB: top_dir is assumed to be in standard Unix
  -- format, '/' separated
  mtool_dir <- liftIO $ findToolDir top_dir
        -- see Note [tooldir: How GHC finds mingw on Windows]

  let installed :: FilePath -> FilePath
      installed file = top_dir </> file
      libexec :: FilePath -> FilePath
      libexec file = top_dir </> "bin" </> file
      settingsFile = installed "settings"

      readFileSafe :: FilePath -> ExceptT SettingsError m String
      readFileSafe path = liftIO (doesFileExist path) >>= \case
        True -> liftIO $ readFile path
        False -> throwE $ SettingsError_MissingData $ "Missing file: " ++ path

  settingsStr <- readFileSafe settingsFile
  settingsList <- case maybeReadFuzzy settingsStr of
    Just s -> pure s
    Nothing -> throwE $ SettingsError_BadData $
      "Can't parse " ++ show settingsFile
  let mySettings = Map.fromList settingsList
  -- See Note [Settings file] for a little more about this file. We're
  -- just partially applying those functions and throwing 'Left's; they're
  -- written in a very portable style to keep ghc-boot light.
  let getSetting key = either pgmError pure $
        getRawFilePathSetting top_dir settingsFile mySettings key
      getToolSetting :: String -> ExceptT SettingsError m String
      getToolSetting key = expandToolDir mtool_dir <$> getSetting key
      getBooleanSetting :: String -> ExceptT SettingsError m Bool
      getBooleanSetting key = either pgmError pure $
        getRawBooleanSetting settingsFile mySettings key
  targetPlatformString <- getSetting "target platform string"
  myExtraGccViaCFlags <- getSetting "GCC extra via C opts"
  -- On Windows, mingw is distributed with GHC,
  -- so we look in TopDir/../mingw/bin,
  -- as well as TopDir/../../mingw/bin for hadrian.
  -- It would perhaps be nice to be able to override this
  -- with the settings file, but it would be a little fiddly
  -- to make that possible, so for now you can't.
  cc_prog <- getToolSetting "C compiler command"
  cc_args_str <- getSetting "C compiler flags"
  cxx_args_str <- getSetting "C++ compiler flags"
  gccSupportsNoPie <- getBooleanSetting "C compiler supports -no-pie"
  cpp_prog <- getToolSetting "Haskell CPP command"
  cpp_args_str <- getSetting "Haskell CPP flags"

  platform <- either pgmError pure $ getTargetPlatform settingsFile mySettings

  let unreg_cc_args = if platformUnregisterised platform
                      then ["-DNO_REGS", "-DUSE_MINIINTERPRETER"]
                      else []
      cpp_args = map Option (words cpp_args_str)
      cc_args  = words cc_args_str ++ unreg_cc_args
      cxx_args = words cxx_args_str
  ldSupportsCompactUnwind <- getBooleanSetting "ld supports compact unwind"
  ldSupportsBuildId       <- getBooleanSetting "ld supports build-id"
  ldSupportsFilelist      <- getBooleanSetting "ld supports filelist"
  ldIsGnuLd               <- getBooleanSetting "ld is GNU ld"

  let globalpkgdb_path = installed "package.conf.d"
      ghc_usage_msg_path  = installed "ghc-usage.txt"
      ghci_usage_msg_path = installed "ghci-usage.txt"

  -- For all systems, unlit, split, mangle are GHC utilities
  -- architecture-specific stuff is done when building Config.hs
  unlit_path <- getToolSetting "unlit command"

  windres_path <- getToolSetting "windres command"
  libtool_path <- getToolSetting "libtool command"
  ar_path <- getToolSetting "ar command"
  otool_path <- getToolSetting "otool command"
  install_name_tool_path <- getToolSetting "install_name_tool command"
  ranlib_path <- getToolSetting "ranlib command"

  touch_path <- getToolSetting "touch command"

  mkdll_prog <- getToolSetting "dllwrap command"
  let mkdll_args = []

  -- cpp is derived from gcc on all platforms
  -- HACK, see setPgmP below. We keep 'words' here to remember to fix
  -- Config.hs one day.


  -- Other things being equal, as and ld are simply gcc
  cc_link_args_str <- getSetting "C compiler link flags"
  let   as_prog  = cc_prog
        as_args  = map Option cc_args
        ld_prog  = cc_prog
        ld_args  = map Option (cc_args ++ words cc_link_args_str)
  ld_r_prog <- getToolSetting "Merge objects command"
  ld_r_args <- getSetting "Merge objects flags"

  llvmTarget <- getSetting "LLVM target"

  -- We just assume on command line
  lc_prog <- getSetting "LLVM llc command"
  lo_prog <- getSetting "LLVM opt command"
  lcc_prog <- getSetting "LLVM clang command"

  let iserv_prog = libexec "ghc-iserv"

  ghcWithInterpreter <- getBooleanSetting "Use interpreter"
  useLibFFI <- getBooleanSetting "Use LibFFI"

  return $ Settings
    { sGhcNameVersion = GhcNameVersion
      { ghcNameVersion_programName = "ghc"
      , ghcNameVersion_projectVersion = cProjectVersion
      }

    , sFileSettings = FileSettings
      { fileSettings_ghcUsagePath   = ghc_usage_msg_path
      , fileSettings_ghciUsagePath  = ghci_usage_msg_path
      , fileSettings_toolDir        = mtool_dir
      , fileSettings_topDir         = top_dir
      , fileSettings_globalPackageDatabase = globalpkgdb_path
      }

    , sToolSettings = ToolSettings
      { toolSettings_ldSupportsCompactUnwind = ldSupportsCompactUnwind
      , toolSettings_ldSupportsBuildId       = ldSupportsBuildId
      , toolSettings_ldSupportsFilelist      = ldSupportsFilelist
      , toolSettings_ldIsGnuLd               = ldIsGnuLd
      , toolSettings_ccSupportsNoPie         = gccSupportsNoPie

      , toolSettings_pgm_L   = unlit_path
      , toolSettings_pgm_P   = (cpp_prog, cpp_args)
      , toolSettings_pgm_F   = ""
      , toolSettings_pgm_c   = cc_prog
      , toolSettings_pgm_a   = (as_prog, as_args)
      , toolSettings_pgm_l   = (ld_prog, ld_args)
      , toolSettings_pgm_lm  = (ld_r_prog, map Option $ words ld_r_args)
      , toolSettings_pgm_dll = (mkdll_prog,mkdll_args)
      , toolSettings_pgm_T   = touch_path
      , toolSettings_pgm_windres = windres_path
      , toolSettings_pgm_libtool = libtool_path
      , toolSettings_pgm_ar = ar_path
      , toolSettings_pgm_otool = otool_path
      , toolSettings_pgm_install_name_tool = install_name_tool_path
      , toolSettings_pgm_ranlib = ranlib_path
      , toolSettings_pgm_lo  = (lo_prog,[])
      , toolSettings_pgm_lc  = (lc_prog,[])
      , toolSettings_pgm_lcc = (lcc_prog,[])
      , toolSettings_pgm_i   = iserv_prog
      , toolSettings_opt_L       = []
      , toolSettings_opt_P       = []
      , toolSettings_opt_P_fingerprint = fingerprint0
      , toolSettings_opt_F       = []
      , toolSettings_opt_c       = cc_args
      , toolSettings_opt_cxx     = cxx_args
      , toolSettings_opt_a       = []
      , toolSettings_opt_l       = []
      , toolSettings_opt_lm      = []
      , toolSettings_opt_windres = []
      , toolSettings_opt_lcc     = []
      , toolSettings_opt_lo      = []
      , toolSettings_opt_lc      = []
      , toolSettings_opt_i       = []

      , toolSettings_extraGccViaCFlags = words myExtraGccViaCFlags
      }

    , sTargetPlatform = platform
    , sPlatformMisc = PlatformMisc
      { platformMisc_targetPlatformString = targetPlatformString
      , platformMisc_ghcWithInterpreter = ghcWithInterpreter
      , platformMisc_libFFI = useLibFFI
      , platformMisc_llvmTarget = llvmTarget
      }

    , sRawSettings    = settingsList
    }

getTargetPlatform
  :: FilePath     -- ^ Settings filepath (for error messages)
  -> RawSettings  -- ^ Raw settings file contents
  -> Either String Platform
getTargetPlatform settingsFile settings = do
  let
    getBooleanSetting = getRawBooleanSetting settingsFile settings
    readSetting :: (Show a, Read a) => String -> Either String a
    readSetting = readRawSetting settingsFile settings

  targetArchOS <- getTargetArchOS settingsFile settings
  targetWordSize <- readSetting "target word size"
  targetWordBigEndian <- getBooleanSetting "target word big endian"
  targetLeadingUnderscore <- getBooleanSetting "Leading underscore"
  targetUnregisterised <- getBooleanSetting "Unregisterised"
  targetHasGnuNonexecStack <- getBooleanSetting "target has GNU nonexec stack"
  targetHasIdentDirective <- getBooleanSetting "target has .ident directive"
  targetHasSubsectionsViaSymbols <- getBooleanSetting "target has subsections via symbols"
  targetHasLibm <- getBooleanSetting "target has libm"
  crossCompiling <- getBooleanSetting "cross compiling"
  tablesNextToCode <- getBooleanSetting "Tables next to code"

  pure $ Platform
    { platformArchOS    = targetArchOS
    , platformWordSize  = targetWordSize
    , platformByteOrder = if targetWordBigEndian then BigEndian else LittleEndian
    , platformUnregisterised = targetUnregisterised
    , platformHasGnuNonexecStack = targetHasGnuNonexecStack
    , platformHasIdentDirective = targetHasIdentDirective
    , platformHasSubsectionsViaSymbols = targetHasSubsectionsViaSymbols
    , platformIsCrossCompiling = crossCompiling
    , platformLeadingUnderscore = targetLeadingUnderscore
    , platformTablesNextToCode  = tablesNextToCode
    , platformHasLibm = targetHasLibm
    , platform_constants = Nothing -- will be filled later when loading (or building) the RTS unit
    }
