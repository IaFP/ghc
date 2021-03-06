module Rules.ToolArgs(toolArgsTarget) where

import qualified Rules.Generate
import Development.Shake
import Target
import Context
import Stage
import Expression

import Packages
import Settings
import Hadrian.Oracles.Cabal
import Hadrian.Haskell.Cabal.Type
import System.Directory (canonicalizePath)

-- | @tool:@ is used by tooling in order to get the arguments necessary
-- to set up a GHC API session which can compile modules from GHC. When
-- run, the target prints out the arguments that would be passed to @ghc@
-- during normal compilation to @stdout@ for the file passed as an
-- argument.
--
-- This target is called by the `ghci.sh` script in order to load all of GHC's
-- modules into GHCi. It is invoked with argument `tool:ghc/Main.hs` in
-- that script so that we can load the whole library and executable
-- components into GHCi.
--
-- In the future where we have multi-component ghci this code can be
-- modified to supply the right arguments for that. At the moment it is
-- also used for GHC's support for multi-component ghcide (see the
-- `hadrian/hie-bios` script).


-- | A phony target of form `tool:path/to/file.hs` which returns the
-- options needed to compile the specific file.
toolArgsTarget :: Rules ()
toolArgsTarget = do
  phonys (\s -> if "tool:" `isPrefixOf` s then Just (toolRuleBody (drop 5 s)) else Nothing)

toolRuleBody :: FilePath -> Action ()
toolRuleBody fp = do
  mm <- dirMap
  cfp <- liftIO $ canonicalizePath fp
  case find (flip isPrefixOf cfp . fst) mm  of
    Just (_, (p, extra)) -> mkToolTarget extra p
    Nothing -> fail $ "No prefixes matched " ++ show fp ++ " IN\n " ++ show mm

mkToolTarget :: [String] -> Package -> Action ()
mkToolTarget es p = do
    -- This builds automatically generated dependencies. Not sure how to do
    -- this generically yet.
    allDeps
    let fake_target = target (Context Stage0 p (if windowsHost then vanilla else dynamic))
                        (Ghc ToolArgs Stage0) [] ["ignored"]
    arg_list <- interpret fake_target getArgs
    liftIO $ putStrLn (intercalate "\n" (arg_list ++ es))
allDeps :: Action ()
allDeps = do
   do
    -- We can't build DLLs on Windows (yet). Actually we should only
    -- include the dynamic way when we have a dynamic host GHC, but just
    -- checking for Windows seems simpler for now.
    let fake_target = target (Context Stage0 compiler (if windowsHost then vanilla else dynamic))
                             (Ghc ToolArgs Stage0) [] ["ignored"]

    -- need the autogenerated files so that they are precompiled
    interpret fake_target Rules.Generate.compilerDependencies >>= need

    root <- buildRoot
    let dir = buildDir (vanillaContext Stage0 compiler)
    need [ root -/- dir -/- "GHC" -/- "Settings" -/- "Config.hs" ]
    need [ root -/- dir -/- "GHC" -/- "Parser.hs" ]
    need [ root -/- dir -/- "GHC" -/- "Parser" -/- "Lexer.hs" ]
    need [ root -/- dir -/- "GHC" -/- "Cmm" -/- "Parser.hs" ]
    need [ root -/- dir -/- "GHC" -/- "Cmm" -/- "Lexer.hs"  ]

-- This list is quite a lot like stage0packages but doesn't include
-- critically the `exe:ghc` component as that depends on the GHC library
-- which takes a while to compile.
toolTargets :: [Package]
toolTargets = [ array
             , bytestring
             , templateHaskell
             , containers
             , deepseq
             , directory
             , exceptions
             , filepath
             , compiler
             , ghcCompact
             , ghcPrim
             --, haskeline
             , hp2ps
             , hsc2hs
             , pretty
             , process
             , rts
             , stm
             , time
             , unlit
             , xhtml ]

-- | Create a mapping from files to which component it belongs to.
dirMap :: Action [(FilePath, (Package, [String]))]
dirMap = do
  auto <- concatMapM go toolTargets
  -- Mush the ghc executable into the compiler component so the whole of ghc is not built when
  -- configuring
  ghc_exe <- mkGhc
  return (auto ++ [ghc_exe])

  where
    -- Make a separate target for the exe:ghc target because otherwise
    -- configuring would build the whole GHC library which we probably
    -- don't want to do.
    mkGhc = do
      let c = (Context Stage0 compiler (if windowsHost then vanilla else dynamic))
      cd <- readContextData c
      fp <- liftIO $ canonicalizePath "ghc/"
      return (fp, (compiler, "-ighc" : modules cd ++ otherModules cd ++ ["ghc/Main.hs"]))
    go p = do
      let c = (Context Stage0 p (if windowsHost then vanilla else dynamic))
      -- readContextData has the effect of configuring the package so all
      -- dependent packages will also be built.
      cd <- readContextData c
      ids <- liftIO $ mapM canonicalizePath [pkgPath p </> i | i <- srcDirs cd]
      return $ map (,(p, modules cd ++ otherModules cd)) ids

