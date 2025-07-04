{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
module HIE.Bios.Cradle (
      findCradle
    , loadCradle
    , loadImplicitCradle
    , yamlConfig
    , defaultCradle
    , isCabalCradle
    , isStackCradle
    , isDirectCradle
    , isBiosCradle
    , isNoneCradle
    , isMultiCradle
    , isDefaultCradle
    , isOtherCradle
    , getCradle
    , Process.readProcessWithOutputs
    , Process.readProcessWithCwd
    , makeCradleResult
    -- | Cradle project configuration types
    , CradleProjectConfig(..)
  ) where

import Colog.Core (LogAction (..), WithSeverity (..), Severity (..), (<&))
import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import qualified Data.Yaml as Yaml
import Data.Version
import Data.Void
import Data.Bifunctor (first)
import Data.Conduit.Process
import Data.Maybe (fromMaybe)
import Data.List
import Data.Ord (Down(..))
import qualified Data.Text as T
import System.Environment
import System.Exit
import System.FilePath
import System.Directory
import System.IO (hClose, hPutStr)
import System.IO.Temp

import HIE.Bios.Config
import HIE.Bios.Types hiding (ActionName(..))
import qualified HIE.Bios.Process as Process
import qualified HIE.Bios.Types as Types
import qualified HIE.Bios.Ghc.Gap as Gap
import HIE.Bios.Cradle.ProjectConfig
import HIE.Bios.Cradle.Utils
import HIE.Bios.Cradle.Cabal as Cabal
import HIE.Bios.Cradle.Resolved
import HIE.Bios.Cradle.ProgramVersions

----------------------------------------------------------------

-- | Given @root\/foo\/bar.hs@, return @root\/hie.yaml@, or wherever the yaml file was found.
--
-- Note, 'findCradle' used to **not** work for directories and required a Haskell file.
-- This has been fixed since @0.14.0@.
-- However, 'loadCradle' and 'loadImplicitCradle' still require a Haskell
-- source file and won't work properly with a directory parameter.
findCradle :: FilePath -> IO (Maybe FilePath)
findCradle wfile = do
    wdir <- doesDirectoryExist wfile >>= \case
      True ->  pure wfile
      False -> pure (takeDirectory wfile)
    runMaybeT (yamlConfig wdir)

-- | Given root\/hie.yaml load the Cradle.
loadCradle :: LogAction IO (WithSeverity Log) -> FilePath -> IO (Cradle Void)
loadCradle l = loadCradleWithOpts l absurd

-- | Given root\/foo\/bar.hs, load an implicit cradle
loadImplicitCradle :: Show a => LogAction IO (WithSeverity Log) -> FilePath -> IO (Cradle a)
loadImplicitCradle l wfile = do
  let wdir = takeDirectory wfile
  cfg <- runMaybeT (implicitConfig wdir)
  case cfg of
    Just bc -> getCradle l absurd bc
    Nothing -> return $ defaultCradle l wdir

-- | Finding 'Cradle'.
--   Find a cabal file by tracing ancestor directories.
--   Find a sandbox according to a cabal sandbox config
--   in a cabal directory.
loadCradleWithOpts :: (Yaml.FromJSON b, Show a) => LogAction IO (WithSeverity Log) -> (b -> CradleAction a) -> FilePath -> IO (Cradle a)
loadCradleWithOpts l buildCustomCradle wfile = do
    cradleConfig <- readCradleConfig wfile
    getCradle l buildCustomCradle (cradleConfig, takeDirectory wfile)

getCradle :: Show a => LogAction IO (WithSeverity Log) ->  (b -> CradleAction a) -> (CradleConfig b, FilePath) -> IO (Cradle a)
getCradle l buildCustomCradle (cc, wdir) = do
    rcs <- canonicalizeResolvedCradles wdir cs
    resolvedCradlesToCradle l buildCustomCradle wdir rcs
  where
    cs = resolveCradleTree wdir cc

addActionDeps :: [FilePath] -> CradleLoadResult ComponentOptions -> CradleLoadResult ComponentOptions
addActionDeps deps =
  cradleLoadResult
      CradleNone
      (\err -> CradleFail (err { cradleErrorDependencies = cradleErrorDependencies err `union` deps }))
      (\(ComponentOptions os' dir ds) -> CradleSuccess (ComponentOptions os' dir (ds `union` deps)))


resolvedCradlesToCradle :: Show a => LogAction IO (WithSeverity Log) -> (b -> CradleAction a) -> FilePath -> [ResolvedCradle b] -> IO (Cradle a)
resolvedCradlesToCradle logger buildCustomCradle root cs = mdo
  let run_ghc_cmd args =
        -- We're being lazy here and just returning the ghc path for the
        -- first non-none cradle. This shouldn't matter in practice: all
        -- sub cradles should be using the same ghc version!
        case filter (notNoneType . actionName) $ map snd cradleActions of
          [] -> return CradleNone
          (act:_) ->
            runGhcCmd
              act
              args
  versions <- makeVersions logger root run_ghc_cmd
  let rcs = ResolvedCradles root cs versions
      cradleActions = [ (c, resolveCradleAction logger buildCustomCradle rcs root c) | c <- cs ]
      err_msg fp
        = ["Multi Cradle: No prefixes matched"
          , "pwd: " ++ root
          , "filepath: " ++ fp
          , "prefixes:"
          ] ++ [show (prefix pf, actionName cc) | (pf, cc) <- cradleActions]
  pure $ Cradle
    { cradleRootDir = root
    , cradleLogger = logger
    , cradleOptsProg = CradleAction
      { actionName = multiActionName
      , runCradle  = \fp prev -> do
          absfp <- makeAbsolute fp
          case selectCradle (prefix . fst) absfp cradleActions of
            Just (rc, act) -> do
              addActionDeps (cradleDeps rc) <$> runCradle act fp prev
            Nothing -> return $ CradleFail $ CradleError [] ExitSuccess (err_msg fp) [fp]
      , runGhcCmd = run_ghc_cmd
      }
    }
  where
    multiActionName
      | all (\c -> isStackCradleConfig c || isNoneCradleConfig c) cs
      = Types.Stack
      | all (\c -> isCabalCradleConfig c || isNoneCradleConfig c) cs
      = Types.Cabal
      | [True] <- map isBiosCradleConfig $ filter (not . isNoneCradleConfig) cs
      = Types.Bios
      | [True] <- map isDirectCradleConfig $ filter (not . isNoneCradleConfig) cs
      = Types.Direct
      | otherwise
      = Types.Multi

    isStackCradleConfig cfg = case concreteCradle cfg of
      ConcreteStack{} -> True
      _               -> False

    isCabalCradleConfig cfg = case concreteCradle cfg of
      ConcreteCabal{} -> True
      _               -> False

    isBiosCradleConfig cfg = case concreteCradle cfg of
      ConcreteBios{}  -> True
      _               -> False

    isDirectCradleConfig cfg = case concreteCradle cfg of
      ConcreteDirect{} -> True
      _                -> False

    isNoneCradleConfig cfg = case concreteCradle cfg of
      ConcreteNone{} -> True
      _              -> False

    notNoneType Types.None = False
    notNoneType _ = True


resolveCradleAction :: Show a => LogAction IO (WithSeverity Log) -> (b -> CradleAction a) -> ResolvedCradles b -> FilePath -> ResolvedCradle b -> CradleAction a
resolveCradleAction l buildCustomCradle cs root cradle = addLoadStyleLogToCradleAction $
  case concreteCradle cradle of
    ConcreteCabal t -> cabalCradle l cs root (cabalComponent t) (projectConfigFromMaybe root (cabalProjectFile t))
    ConcreteStack t -> stackCradle l root (stackComponent t) (projectConfigFromMaybe root (stackYaml t))
    ConcreteBios bios deps mbGhc -> biosCradle l cs root bios deps mbGhc
    ConcreteDirect xs -> directCradle l root xs
    ConcreteNone -> noneCradle
    ConcreteOther a -> buildCustomCradle a
  where
    -- Add a log message to each loading operation.
    addLoadStyleLogToCradleAction crdlAct = crdlAct
      { runCradle = \fp ls -> do
          l <& LogRequestedCradleLoadStyle (T.pack $ show $ actionName crdlAct) ls `WithSeverity` Debug
          runCradle crdlAct fp ls
      }

resolveCradleTree :: FilePath -> CradleConfig a -> [ResolvedCradle a]
resolveCradleTree root (CradleConfig confDeps confTree) = go root confDeps confTree
  where
    go pfix deps tree = case tree of
      Cabal t              -> [ResolvedCradle pfix deps (ConcreteCabal t)]
      Stack t              -> [ResolvedCradle pfix deps (ConcreteStack t)]
      Bios bios dcmd mbGhc -> [ResolvedCradle pfix deps (ConcreteBios bios dcmd mbGhc)]
      Direct xs            -> [ResolvedCradle pfix deps (ConcreteDirect xs)]
      None                 -> [ResolvedCradle pfix deps ConcreteNone]
      Other a _            -> [ResolvedCradle pfix deps (ConcreteOther a)]
      CabalMulti dc xs     -> [ResolvedCradle p    deps (ConcreteCabal (dc <> c)) | (p, c) <- xs ]
      StackMulti dc xs     -> [ResolvedCradle p    deps (ConcreteStack (dc <> c)) | (p, c) <- xs ]
      Multi xs             -> concat [ go pfix' (deps ++ deps') tree' | (pfix', CradleConfig deps' tree') <- xs]

-- | Try to infer an appropriate implicit cradle type from stuff we can find in the enclosing directories:
--   * If a .hie-bios file is found, we can treat this as a @Bios@ cradle
--   * If a stack.yaml file is found, we can treat this as a @Stack@ cradle
--   * If a cabal.project or an xyz.cabal file is found, we can treat this as a @Cabal@ cradle
inferCradleTree :: FilePath -> MaybeT IO (CradleTree a, FilePath)
inferCradleTree fp =
       maybeItsBios
   <|> maybeItsStack
   <|> maybeItsCabal
-- <|> maybeItsObelisk
-- <|> maybeItsObelisk

  where
  maybeItsBios = (\wdir -> (Bios (Program $ wdir </> ".hie-bios") Nothing Nothing, wdir)) <$> biosWorkDir fp

  maybeItsStack = stackExecutable >> (Stack $ StackType Nothing Nothing,) <$> stackWorkDir fp

  maybeItsCabal = (Cabal $ CabalType Nothing Nothing,) <$> cabalWorkDir fp

  -- maybeItsObelisk = (Obelisk,) <$> obeliskWorkDir fp

  -- maybeItsBazel = (Bazel,) <$> rulesHaskellWorkDir fp


-- | Wraps up the cradle inferred by @inferCradleTree@ as a @CradleConfig@ with no dependencies
implicitConfig :: FilePath -> MaybeT IO (CradleConfig a, FilePath)
implicitConfig = (fmap . first) (CradleConfig noDeps) . inferCradleTree
  where
  noDeps :: [FilePath]
  noDeps = []

yamlConfig :: FilePath ->  MaybeT IO FilePath
yamlConfig fp = do
  configDir <- yamlConfigDirectory fp
  return (configDir </> configFileName)

yamlConfigDirectory :: FilePath -> MaybeT IO FilePath
yamlConfigDirectory = Process.findFileUpwards configFileName

readCradleConfig :: Yaml.FromJSON b => FilePath -> IO (CradleConfig b)
readCradleConfig yamlHie = do
  cfg  <- liftIO $ readConfig yamlHie
  return (cradle cfg)

configFileName :: FilePath
configFileName = "hie.yaml"

-- | Pass '-dynamic' flag when GHC is built with dynamic linking.
--
-- Append flag to options of 'defaultCradle' and 'directCradle' if GHC is dynmically linked,
-- because unlike the case of using build tools, which means '-dynamic' can be set via
-- '.cabal' or 'package.yaml', users have to create an explicit hie.yaml to pass this flag.
argDynamic :: [String]
argDynamic = ["-dynamic" | Gap.hostIsDynamic ]

---------------------------------------------------------------

isCabalCradle :: Cradle a -> Bool
isCabalCradle crdl = case actionName (cradleOptsProg crdl) of
  Types.Cabal -> True
  _ -> False

isStackCradle :: Cradle a -> Bool
isStackCradle crdl = case actionName (cradleOptsProg crdl) of
  Types.Stack -> True
  _ -> False

isDirectCradle :: Cradle a -> Bool
isDirectCradle crdl = case actionName (cradleOptsProg crdl) of
  Types.Direct -> True
  _ -> False

isBiosCradle :: Cradle a -> Bool
isBiosCradle crdl = case actionName (cradleOptsProg crdl) of
  Types.Bios -> True
  _ -> False

isMultiCradle :: Cradle a -> Bool
isMultiCradle crdl = case actionName (cradleOptsProg crdl) of
  Types.Multi -> True
  _ -> False

isNoneCradle :: Cradle a -> Bool
isNoneCradle crdl = case actionName (cradleOptsProg crdl) of
  Types.None -> True
  _ -> False

isDefaultCradle :: Cradle a -> Bool
isDefaultCradle crdl = case actionName (cradleOptsProg crdl) of
  Types.Default -> True
  _ -> False

isOtherCradle :: Cradle a -> Bool
isOtherCradle crdl = case actionName (cradleOptsProg crdl) of
  Types.Other _ -> True
  _ -> False

---------------------------------------------------------------

-- | Default cradle has no special options, not very useful for loading
-- modules.
defaultCradle :: LogAction IO (WithSeverity Log) -> FilePath -> Cradle a
defaultCradle l cur_dir =
  Cradle
    { cradleRootDir = cur_dir
    , cradleLogger = l
    , cradleOptsProg = CradleAction
        { actionName = Types.Default
        , runCradle = \_ _ ->
            return (CradleSuccess (ComponentOptions argDynamic cur_dir []))
        , runGhcCmd = runGhcCmdOnPath l cur_dir
        }
    }

---------------------------------------------------------------
-- | The none cradle tells us not to even attempt to load a certain directory

noneCradle :: CradleAction a
noneCradle =
  CradleAction
      { actionName = Types.None
      , runCradle = \_ _ -> return CradleNone
      , runGhcCmd = \_ -> return CradleNone
      }

---------------------------------------------------------------
-- | The multi cradle selects a cradle based on the filepath
--
-- Canonicalize the relative paths present in the multi-cradle and
-- also order the paths by most specific first. In the cradle selection
-- function we want to choose the most specific cradle possible.
canonicalizeResolvedCradles :: FilePath -> [ResolvedCradle a] -> IO [ResolvedCradle a]
canonicalizeResolvedCradles cur_dir cs =
  sortOn (Down . prefix)
    <$> mapM (\c -> (\abs_fp -> c {prefix = abs_fp}) <$> makeAbsolute (cur_dir </> prefix c)) cs

-------------------------------------------------------------------------

directCradle :: LogAction IO (WithSeverity Log) -> FilePath -> [String] -> CradleAction a
directCradle l wdir args
  = CradleAction
      { actionName = Types.Direct
      , runCradle = \_ loadStyle -> do
          logCradleHasNoSupportForLoadWithContext l loadStyle "direct"
          return (CradleSuccess (ComponentOptions (args ++ argDynamic) wdir []))
      , runGhcCmd = runGhcCmdOnPath l wdir
      }


-------------------------------------------------------------------------


-- | Find a cradle by finding an executable `hie-bios` file which will
-- be executed to find the correct GHC options to use.
biosCradle :: LogAction IO (WithSeverity Log) -> ResolvedCradles b -> FilePath -> Callable -> Maybe Callable -> Maybe FilePath -> CradleAction a
biosCradle l rc wdir biosCall biosDepsCall mbGhc
  = CradleAction
      { actionName = Types.Bios
      , runCradle = biosAction rc wdir biosCall biosDepsCall l
      , runGhcCmd = \args -> Process.readProcessWithCwd l wdir (fromMaybe "ghc" mbGhc) args ""
      }

biosWorkDir :: FilePath -> MaybeT IO FilePath
biosWorkDir = Process.findFileUpwards ".hie-bios"

biosDepsAction :: LogAction IO (WithSeverity Log) -> FilePath -> Maybe Callable -> FilePath -> LoadStyle -> IO [FilePath]
biosDepsAction l wdir (Just biosDepsCall) fp loadStyle = do
  let fps = case loadStyle of
        LoadFile -> [fp]
        LoadWithContext old_fps -> fp : old_fps
  (ex, sout, serr, [(_, args)]) <-
    runContT (withCallableToProcess biosDepsCall fps) $ \biosDeps' ->
      Process.readProcessWithOutputs [hie_bios_output] l wdir biosDeps'
  case ex of
    ExitFailure _ ->  error $ show (ex, sout, serr)
    ExitSuccess -> return $ fromMaybe [] args
biosDepsAction _ _ Nothing _ _ = return []

biosAction
  :: ResolvedCradles a
  -> FilePath
  -> Callable
  -> Maybe Callable
  -> LogAction IO (WithSeverity Log)
  -> FilePath
  -> LoadStyle
  -> IO (CradleLoadResult ComponentOptions)
biosAction rc wdir bios bios_deps l fp loadStyle = do
  ghc_version <- liftIO $ runCachedIO $ ghcVersion $ cradleProgramVersions rc
  determinedLoadStyle <- case ghc_version of
    Just ghc
      -- Multi-component supported from ghc 9.4
      -- We trust the assertion for a bios program, as we have no way of
      -- checking its version
      | LoadWithContext _ <- loadStyle ->
          if ghc >= makeVersion [9,4]
            then pure loadStyle
            else do
              liftIO $ l <& WithSeverity
                (LogLoadWithContextUnsupported "bios"
                  $ Just "ghc version is too old. We require `ghc >= 9.4`"
                )
                Warning
              pure LoadFile
    _ -> pure LoadFile
  let fps = case determinedLoadStyle of
        LoadFile -> [fp]
        LoadWithContext old_fps -> fp : old_fps
  (ex, _stdo, std, [(_, res),(_, mb_deps)]) <-
    runContT (withCallableToProcess bios fps) $ \bios' ->
      Process.readProcessWithOutputs [hie_bios_output, hie_bios_deps] l wdir bios'

  deps <- case mb_deps of
    Just x  -> return x
    Nothing -> biosDepsAction l wdir bios_deps fp loadStyle
        -- Output from the program should be written to the output file and
        -- delimited by newlines.
        -- Execute the bios action and add dependencies of the cradle.
        -- Removes all duplicates.
  return $ makeCradleResult (ex, std, wdir, fromMaybe [] res) deps [fp]

withCallableToProcess :: Callable -> [String] -> ContT a IO CreateProcess
withCallableToProcess (Command shellCommand) files = ContT $ \action -> do
  old_env <- getEnvironment
  case files of
    [] -> action $ (shell shellCommand) {env = Nothing}
    (x : _) ->
      runContT (withHieBiosMultiArg files) $ \multi_file -> do
        let updated_env = Just $
              [ (hie_bios_multi_arg, multi_file)
              , (hie_bios_arg, x)
              ] ++
              old_env
        action $ (shell shellCommand){env = updated_env}
withCallableToProcess (Program path) files = ContT $ \action -> do
  canon_path <- canonicalizePath path
  old_env <- getEnvironment
  case files of
    [] -> action $ (proc canon_path []){env = Nothing}
    (x : _) ->
      runContT (withHieBiosMultiArg files) $ \multi_file -> do
        let updated_env = Just $
              (hie_bios_multi_arg, multi_file) : old_env
        action $ (proc canon_path [x]){env = updated_env}

withHieBiosMultiArg :: [String] -> ContT a IO FilePath
withHieBiosMultiArg files = ContT $ \action -> do
  withSystemTempFile hie_bios_multi_arg $ \file h -> do
    case files of
      [] -> hClose h >> action file
      (f0 : rest) -> do
        hPutStr h f0
        forM_ rest $ \f -> hPutStr h "\x00" >> hPutStr h f
        hClose h
        action file

------------------------------------------------------------------------

-- |Cabal Cradle
-- Works for new-build by invoking `v2-repl`.
cabalCradle :: LogAction IO (WithSeverity Log) -> ResolvedCradles b -> FilePath -> Maybe String -> CradleProjectConfig -> CradleAction a
cabalCradle l cs wdir mc projectFile
  = CradleAction
    { actionName = Types.Cabal
    , runCradle = \fp -> runCradleResultT . cabalAction cs wdir mc l projectFile fp
    , runGhcCmd = runCabalGhcCmd cs wdir l projectFile
    }

cabalWorkDir :: FilePath -> MaybeT IO FilePath
cabalWorkDir wdir =
      Process.findFileUpwards "cabal.project" wdir
  <|> Process.findFileUpwardsPredicate (\fp -> takeExtension fp == ".cabal") wdir

------------------------------------------------------------------------

stackYamlProcessArgs :: CradleProjectConfig -> [String]
stackYamlProcessArgs (ExplicitConfig yaml) = ["--stack-yaml", yaml]
stackYamlProcessArgs NoExplicitConfig = []

stackYamlLocationOrDefault :: CradleProjectConfig -> FilePath
stackYamlLocationOrDefault NoExplicitConfig = "stack.yaml"
stackYamlLocationOrDefault (ExplicitConfig yaml) = yaml

-- | Stack Cradle
-- Works for by invoking `stack repl` with a wrapper script
stackCradle :: LogAction IO (WithSeverity Log) ->  FilePath -> Maybe String -> CradleProjectConfig -> CradleAction a
stackCradle l wdir mc syaml =
  CradleAction
    { actionName = Types.Stack
    , runCradle = stackAction wdir mc syaml l
    , runGhcCmd = \args -> runCradleResultT $ do
        -- Setup stack silently, since stack might print stuff to stdout in some cases (e.g. on Win)
        -- Issue 242 from HLS: https://github.com/haskell/haskell-language-server/issues/242
        _ <- Process.readProcessWithCwd_ l wdir "stack" (stackYamlProcessArgs syaml <> ["setup", "--silent"]) ""
        Process.readProcessWithCwd_ l wdir "stack"
          (stackYamlProcessArgs syaml <> ["exec", "ghc", "--"] <> args)
          ""
    }

-- | @'stackCradleDependencies' rootDir componentDir@.
-- Compute the dependencies of the stack cradle based
-- on the cradle root and the component directory.
--
-- Directory 'componentDir' is a sub-directory where we look for
-- package specific cradle dependencies, such as 'package.yaml' and
-- a '.cabal' file.
--
-- Found dependencies are relative to 'rootDir'.
stackCradleDependencies :: FilePath -> FilePath -> CradleProjectConfig -> IO [FilePath]
stackCradleDependencies wdir componentDir syaml = do
  let relFp = makeRelative wdir componentDir
  cabalFiles' <- findCabalFiles componentDir
  let cabalFiles = map (relFp </>) cabalFiles'
  return $ map normalise $
    cabalFiles ++ [relFp </> "package.yaml", stackYamlLocationOrDefault syaml]

stackAction
  :: FilePath
  -> Maybe String
  -> CradleProjectConfig
  -> LogAction IO (WithSeverity Log)
  -> FilePath
  -> LoadStyle
  -> IO (CradleLoadResult ComponentOptions)
stackAction workDir mc syaml l fp loadStyle = do
  logCradleHasNoSupportForLoadWithContext l loadStyle "stack"
  let ghcProc args = proc "stack" (stackYamlProcessArgs syaml <> ["exec", "ghc", "--"] <> args)
  -- Same wrapper works as with cabal
  wrapper_fp <- withGhcWrapperTool l ghcProc workDir
  (ex1, _stdo, stde, [(_, maybeArgs)]) <-
    Process.readProcessWithOutputs [hie_bios_output] l workDir
      $ stackProcess syaml
          $  ["repl", "--no-nix-pure", "--with-ghc", wrapper_fp]
          <> [ comp | Just comp <- [mc] ]

  (ex2, pkg_args, stdr, _) <-
    Process.readProcessWithOutputs [hie_bios_output] l workDir
      $ stackProcess syaml ["path", "--ghc-package-path"]

  let split_pkgs = concatMap splitSearchPath pkg_args
      pkg_ghc_args = concatMap (\p -> ["-package-db", p] ) split_pkgs
      args = fromMaybe [] maybeArgs
  case processCabalWrapperArgs args of
      Nothing -> do
        -- Best effort. Assume the working directory is the
        -- the root of the component, so we are right in trivial cases at least.
        deps <- stackCradleDependencies workDir workDir syaml
        pure $ CradleFail
                  (CradleError deps ex1
                    ([ "Failed to parse result of calling stack" ]
                    ++ stde
                    ++ args)
                    [fp]
                  )

      Just (componentDir, ghc_args) -> do
        deps <- stackCradleDependencies workDir componentDir syaml
        pure $ makeCradleResult
                  ( combineExitCodes [ex1, ex2]
                  , stde ++ stdr, componentDir
                  , ghc_args ++ pkg_ghc_args
                  )
                  deps
                  [fp]

stackProcess :: CradleProjectConfig -> [String] -> CreateProcess
stackProcess syaml args = proc "stack" $ stackYamlProcessArgs syaml <> args

combineExitCodes :: [ExitCode] -> ExitCode
combineExitCodes = foldr go ExitSuccess
  where
    go ExitSuccess b = b
    go a _ = a

stackExecutable :: MaybeT IO FilePath
stackExecutable = MaybeT $ findExecutable "stack"

stackWorkDir :: FilePath -> MaybeT IO FilePath
stackWorkDir = Process.findFileUpwards "stack.yaml"

{-
-- Support removed for 0.3 but should be added back in the future
----------------------------------------------------------------------------
-- rules_haskell - Thanks for David Smith for helping with this one.
-- Looks for the directory containing a WORKSPACE file
--
rulesHaskellWorkDir :: FilePath -> MaybeT IO FilePath
rulesHaskellWorkDir fp =
  findFileUpwards "WORKSPACE" fp

rulesHaskellCradle :: FilePath -> Cradle
rulesHaskellCradle wdir =
  Cradle
    { cradleRootDir  = wdir
    , cradleOptsProg   = CradleAction
        { actionName = "bazel"
        , runCradle = rulesHaskellAction wdir
        }
    }

rulesHaskellCradleDependencies :: FilePath -> IO [FilePath]
rulesHaskellCradleDependencies _wdir = return ["BUILD.bazel", "WORKSPACE"]

bazelCommand :: String
bazelCommand = $(embedStringFile "wrappers/bazel")

rulesHaskellAction :: FilePath -> FilePath -> IO (CradleLoadResult ComponentOptions)
rulesHaskellAction workDir fp = do
  wrapper_fp <- writeSystemTempFile "wrapper" bazelCommand
  setFileMode wrapper_fp accessModes
  let rel_path = makeRelative workDir fp
  (ex, args, stde) <-
      readProcessWithOutputFile workDir wrapper_fp [rel_path] []
  let args'  = filter (/= '\'') args
  let args'' = filter (/= "\"$GHCI_LOCATION\"") (words args')
  deps <- rulesHaskellCradleDependencies workDir
  return $ makeCradleResult (ex, stde, args'') deps


------------------------------------------------------------------------------
-- Obelisk Cradle
-- Searches for the directory which contains `.obelisk`.

obeliskWorkDir :: FilePath -> MaybeT IO FilePath
obeliskWorkDir fp = do
  -- Find a possible root which will contain the cabal.project
  wdir <- findFileUpwards (== "cabal.project") fp
  -- Check for the ".obelisk" folder in this directory
  check <- liftIO $ doesDirectoryExist (wdir </> ".obelisk")
  unless check (fail "Not obelisk dir")
  return wdir

obeliskCradleDependencies :: FilePath -> IO [FilePath]
obeliskCradleDependencies _wdir = return []

obeliskCradle :: FilePath -> Cradle
obeliskCradle wdir =
  Cradle
    { cradleRootDir  = wdir
    , cradleOptsProg = CradleAction
        { actionName = "obelisk"
        , runCradle = obeliskAction wdir
        }
    }

obeliskAction :: FilePath -> FilePath -> IO (CradleLoadResult ComponentOptions)
obeliskAction workDir _fp = do
  (ex, args, stde) <-
      readProcessWithOutputFile workDir "ob" ["ide-args"] []

  o_deps <- obeliskCradleDependencies workDir
  return (makeCradleResult (ex, stde, words args) o_deps )

-}

makeCradleResult :: (ExitCode, [String], FilePath, [String]) -> [FilePath] -> [FilePath] -> CradleLoadResult ComponentOptions
makeCradleResult (ex, err, componentDir, gopts) deps loadingFiles =
  case ex of
    ExitFailure _ -> CradleFail (CradleError deps ex err loadingFiles)
    _ ->
        let compOpts = ComponentOptions gopts componentDir deps
        in CradleSuccess compOpts

-- | Calls @ghc --print-libdir@, with just whatever's on the PATH.
runGhcCmdOnPath :: LogAction IO (WithSeverity Log) -> FilePath -> [String] -> IO (CradleLoadResult String)
runGhcCmdOnPath l wdir args = Process.readProcessWithCwd l wdir "ghc" args ""

-- | Log that the cradle has no supported for loading with context, if and only if
-- 'LoadWithContext' was requested.
logCradleHasNoSupportForLoadWithContext :: Applicative m => LogAction m (WithSeverity Log) -> LoadStyle -> T.Text -> m ()
logCradleHasNoSupportForLoadWithContext l (LoadWithContext _) crdlName =
  l <& WithSeverity
        (LogLoadWithContextUnsupported crdlName
          $ Just $ crdlName <> " doesn't support loading multiple components at once"
        )
        Info
logCradleHasNoSupportForLoadWithContext _ _ _ = pure ()
