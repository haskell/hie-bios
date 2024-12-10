{-# LANGUAGE BangPatterns #-}
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
    , readProcessWithOutputs
    , readProcessWithCwd
    , makeCradleResult
    -- | Cradle project configuration types
    , CradleProjectConfig(..)
  ) where

import Control.Applicative ((<|>), optional)
import Data.Bifunctor (first)
import Control.DeepSeq
import Control.Exception (handleJust)
import qualified Data.Yaml as Yaml
import Data.Void
import Data.Char (isSpace)
import System.Exit
import System.Directory hiding (findFile)
import Colog.Core (LogAction (..), WithSeverity (..), Severity (..), (<&))
import Control.Monad
import Control.Monad.Extra (unlessM)
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Data.Conduit.Process
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit as C
import qualified Data.Conduit.Text as C
import qualified Data.HashMap.Strict as Map
import Data.Maybe (fromMaybe)
import Data.List
import Data.List.Extra (trimEnd)
import Data.Ord (Down(..))
import qualified Data.Text as T
import System.Environment
import System.FilePath
import System.PosixCompat.Files
import System.Info.Extra (isWindows)
import System.IO (hClose, hGetContents, hSetBuffering, BufferMode(LineBuffering), withFile, IOMode(..))
import System.IO.Error (isPermissionError)
import System.IO.Temp

import HIE.Bios.Config
import HIE.Bios.Environment (getCacheDir)
import HIE.Bios.Types hiding (ActionName(..))
import HIE.Bios.Wrappers
import qualified HIE.Bios.Types as Types
import qualified HIE.Bios.Ghc.Gap as Gap

import GHC.Fingerprint (fingerprintString)
import GHC.ResponseFile (escapeArgs)

import Data.Version
import Data.IORef
import Text.ParserCombinators.ReadP (readP_to_S)

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


-- | The actual type of action we will be using to process a file
data ConcreteCradle a
  = ConcreteCabal CabalType
  | ConcreteStack StackType
  | ConcreteBios Callable (Maybe Callable) (Maybe FilePath)
  | ConcreteDirect [String]
  | ConcreteNone
  | ConcreteOther a
  deriving Show

-- | ConcreteCradle augmented with information on which file the
-- cradle applies
data ResolvedCradle a
 = ResolvedCradle
 { prefix :: FilePath -- ^ the prefix to match files
 , cradleDeps :: [FilePath] -- ^ accumulated dependencies
 , concreteCradle :: ConcreteCradle a
 } deriving Show

-- | The final cradle config that specifies the cradle for
-- each prefix we know how to handle
data ResolvedCradles a
 = ResolvedCradles
 { cradleRoot :: FilePath
 , resolvedCradles :: [ResolvedCradle a] -- ^ In order of decreasing specificity
 , cradleProgramVersions :: ProgramVersions
 }

data ProgramVersions =
  ProgramVersions { cabalVersion  :: CachedIO (Maybe Version)
                  , stackVersion  :: CachedIO (Maybe Version)
                  , ghcVersion    :: CachedIO (Maybe Version)
                  }

newtype CachedIO a = CachedIO (IORef (Either (IO a) a))

makeCachedIO :: IO a -> IO (CachedIO a)
makeCachedIO act = CachedIO <$> newIORef (Left act)

runCachedIO :: CachedIO a -> IO a
runCachedIO (CachedIO ref) =
  readIORef ref >>= \case
    Right x -> pure x
    Left act -> do
      x <- act
      writeIORef ref (Right x)
      pure x

makeVersions :: LogAction IO (WithSeverity Log) -> FilePath -> ([String] -> IO (CradleLoadResult String)) -> IO ProgramVersions
makeVersions l wdir ghc = do
  cabalVersion <- makeCachedIO $ getCabalVersion l wdir
  stackVersion <- makeCachedIO $ getStackVersion l wdir
  ghcVersion   <- makeCachedIO $ getGhcVersion ghc
  pure ProgramVersions{..}

getCabalVersion :: LogAction IO (WithSeverity Log) -> FilePath -> IO (Maybe Version)
getCabalVersion l wdir = do
  res <- readProcessWithCwd l wdir "cabal" ["--numeric-version"] ""
  case res of
    CradleSuccess stdo ->
      pure $ versionMaybe stdo
    _ -> pure Nothing

getStackVersion :: LogAction IO (WithSeverity Log) -> FilePath -> IO (Maybe Version)
getStackVersion l wdir = do
  res <- readProcessWithCwd l wdir "stack" ["--numeric-version"] ""
  case res of
    CradleSuccess stdo ->
      pure $ versionMaybe stdo
    _ -> pure Nothing

getGhcVersion :: ([String] -> IO (CradleLoadResult String)) -> IO (Maybe Version)
getGhcVersion ghc = do
  res <- ghc ["--numeric-version"]
  case res of
    CradleSuccess stdo ->
      pure $ versionMaybe stdo
    _ -> pure Nothing

versionMaybe :: String -> Maybe Version
versionMaybe xs = case reverse $ readP_to_S parseVersion xs of
  [] -> Nothing
  (x:_) -> Just (fst x)


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
            Nothing -> return $ CradleFail $ CradleError [] ExitSuccess (err_msg fp)
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
yamlConfigDirectory = findFileUpwards (configFileName ==)

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

-- Canonicalize the relative paths present in the multi-cradle and
-- also order the paths by most specific first. In the cradle selection
-- function we want to choose the most specific cradle possible.
canonicalizeResolvedCradles :: FilePath -> [ResolvedCradle a] -> IO [ResolvedCradle a]
canonicalizeResolvedCradles cur_dir cs =
  sortOn (Down . prefix)
    <$> mapM (\c -> (\abs_fp -> c {prefix = abs_fp}) <$> makeAbsolute (cur_dir </> prefix c)) cs

selectCradle :: (a -> FilePath) -> FilePath -> [a] -> Maybe a
selectCradle _ _ [] = Nothing
selectCradle k cur_fp (c: css) =
    if k c `isPrefixOf` cur_fp
      then Just c
      else selectCradle k cur_fp css


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
      , runGhcCmd = \args -> readProcessWithCwd l wdir (fromMaybe "ghc" mbGhc) args ""
      }

biosWorkDir :: FilePath -> MaybeT IO FilePath
biosWorkDir = findFileUpwards (".hie-bios" ==)

biosDepsAction :: LogAction IO (WithSeverity Log) -> FilePath -> Maybe Callable -> FilePath -> LoadStyle -> IO [FilePath]
biosDepsAction l wdir (Just biosDepsCall) fp loadStyle = do
  let fps = case loadStyle of
        LoadFile -> [fp]
        LoadWithContext old_fps -> fp : old_fps
  biosDeps' <- callableToProcess biosDepsCall fps
  (ex, sout, serr, [(_, args)]) <- readProcessWithOutputs [hie_bios_output] l wdir biosDeps'
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
  bios' <- callableToProcess bios fps
  (ex, _stdo, std, [(_, res),(_, mb_deps)]) <-
    readProcessWithOutputs [hie_bios_output, hie_bios_deps] l wdir bios'

  deps <- case mb_deps of
    Just x  -> return x
    Nothing -> biosDepsAction l wdir bios_deps fp loadStyle
        -- Output from the program should be written to the output file and
        -- delimited by newlines.
        -- Execute the bios action and add dependencies of the cradle.
        -- Removes all duplicates.
  return $ makeCradleResult (ex, std, wdir, fromMaybe [] res) deps

callableToProcess :: Callable -> [String] -> IO CreateProcess
callableToProcess (Command shellCommand) files = do
  old_env <- getEnvironment
  let maybeArg = case files of
        [] -> Nothing
        _  -> Just $ "\0" `intercalate` files
  return $ (shell shellCommand) { env = (: old_env) . (,) hie_bios_arg <$> maybeArg }
callableToProcess (Program path) files = do
  canon_path <- canonicalizePath path
  return $ proc canon_path files

------------------------------------------------------------------------

projectFileProcessArgs :: CradleProjectConfig -> [String]
projectFileProcessArgs (ExplicitConfig prjFile) = ["--project-file", prjFile]
projectFileProcessArgs NoExplicitConfig = []

projectLocationOrDefault :: CradleProjectConfig -> [FilePath]
projectLocationOrDefault = \case
  NoExplicitConfig -> ["cabal.project", "cabal.project.local"]
  (ExplicitConfig prjFile) -> [prjFile, prjFile <.> "local"]

-- |Cabal Cradle
-- Works for new-build by invoking `v2-repl`.
cabalCradle :: LogAction IO (WithSeverity Log) -> ResolvedCradles b -> FilePath -> Maybe String -> CradleProjectConfig -> CradleAction a
cabalCradle l cs wdir mc projectFile
  = CradleAction
    { actionName = Types.Cabal
    , runCradle = \fp -> runCradleResultT . cabalAction cs wdir mc l projectFile fp
    , runGhcCmd = \args -> runCradleResultT $ do
        buildDir <- liftIO $ cabalBuildDir wdir
        -- Workaround for a cabal-install bug on 3.0.0.0:
        -- ./dist-newstyle/tmp/environment.-24811: createDirectory: does not exist (No such file or directory)
        liftIO $ createDirectoryIfMissing True (buildDir </> "tmp")
        -- Need to pass -v0 otherwise we get "resolving dependencies..."
        cabalProc <- cabalProcess l projectFile wdir "v2-exec" $ ["ghc", "-v0", "--"] ++ args
        readProcessWithCwd' l cabalProc ""
    }


-- | Execute a cabal process in our custom cache-build directory configured
-- with the custom ghc executable.
-- The created process has its working directory set to the given working directory.
--
-- Invokes the cabal process in the given directory.
-- Finds the appropriate @ghc@ version as a fallback and provides the path
-- to the custom ghc wrapper via 'hie_bios_ghc' environment variable which
-- the custom ghc wrapper may use as a fallback if it can not respond to certain
-- queries, such as ghc version or location of the libdir.
cabalProcess :: LogAction IO (WithSeverity Log) -> CradleProjectConfig -> FilePath -> String -> [String] -> CradleLoadResultT IO CreateProcess
cabalProcess l cabalProject workDir command args = do
  ghcDirs <- cabalGhcDirs l cabalProject workDir
  newEnvironment <- liftIO $ setupEnvironment ghcDirs
  cabalProc <- liftIO $ setupCabalCommand ghcDirs
  pure $ (cabalProc
      { env = Just newEnvironment
      , cwd = Just workDir
      })
  where
    processEnvironment :: (FilePath, FilePath) -> [(String, String)]
    processEnvironment (ghcBin, libdir) =
      [(hie_bios_ghc, ghcBin), (hie_bios_ghc_args,  "-B" ++ libdir)]

    setupEnvironment :: (FilePath, FilePath) -> IO [(String, String)]
    setupEnvironment ghcDirs = do
      environment <- getCleanEnvironment
      pure $ processEnvironment ghcDirs ++ environment

    setupCabalCommand :: (FilePath, FilePath) -> IO CreateProcess
    setupCabalCommand (ghcBin, libdir) = do
      wrapper_fp <- withGhcWrapperTool l ("ghc", []) workDir
      buildDir <- cabalBuildDir workDir
      ghcPkgPath <- withGhcPkgTool ghcBin libdir
      let extraCabalArgs =
            [ "--builddir=" <> buildDir
            , command
            , "--with-compiler", wrapper_fp
            , "--with-hc-pkg", ghcPkgPath
            ] <> projectFileProcessArgs cabalProject
      pure $ proc "cabal" (extraCabalArgs ++ args)

-- | Discovers the location of 'ghc-pkg' given the absolute path to 'ghc'
-- and its '$libdir' (obtainable by running @ghc --print-libdir@).
--
-- @'withGhcPkgTool' ghcPathAbs libdir@ guesses the location by looking at
-- the filename of 'ghcPathAbs' and expects that 'ghc-pkg' is right next to it,
-- which is guaranteed by the ghc build system. Most OS's follow this
-- convention.
--
-- On unix, there is a high-chance that the obtained 'ghc' location is the
-- "unwrapped" executable, e.g. the executable without a shim that specifies
-- the '$libdir' and other important constants.
-- As such, the executable 'ghc-pkg' is similarly without a wrapper shim and
-- is lacking certain constants such as 'global-package-db'. It is, therefore,
-- not suitable to pass in to other consumers, such as 'cabal'.
--
-- Here, we restore the wrapper-shims, if necessary, thus the returned filepath
-- can be passed to 'cabal' without further modifications.
withGhcPkgTool :: FilePath -> FilePath -> IO FilePath
withGhcPkgTool ghcPathAbs libdir = do
  let ghcName = takeFileName ghcPathAbs
      -- TODO: check for existence
      ghcPkgPath = guessGhcPkgFromGhc ghcName
  if isWindows
    then pure ghcPkgPath
    else withWrapperTool ghcPkgPath
  where
    ghcDir = takeDirectory ghcPathAbs

    guessGhcPkgFromGhc ghcName =
      let ghcPkgName = T.replace "ghc" "ghc-pkg" (T.pack ghcName)
      in ghcDir </> T.unpack ghcPkgName

    -- Only on unix, creates a wrapper script that's hopefully identical
    -- to the wrapper script 'ghc-pkg' usually comes with.
    --
    -- 'ghc-pkg' needs to know the 'global-package-db' location which is
    -- passed in via a wrapper shim that basically wraps 'ghc-pkg' and
    -- only passes in the correct 'global-package-db'.
    -- For an example on how the wrapper script is supposed to look like, take
    -- a look at @cat $(which ghc-pkg)@, assuming 'ghc-pkg' is on your $PATH.
    --
    -- If we used the raw executable, i.e. not wrapped in a shim, then 'cabal'
    -- can not use the given 'ghc-pkg'.
    withWrapperTool ghcPkg = do
      let globalPackageDb = libdir </> "package.conf.d"
          -- This is the same as the wrapper-shims ghc-pkg usually comes with.
          contents = unlines
            [ "#!/bin/sh"
            , unwords ["exec", escapeFilePath ghcPkg
                      , "--global-package-db", escapeFilePath globalPackageDb
                      , "${1+\"$@\"}"
                      ]
            ]
          srcHash = show (fingerprintString contents)
      cacheFile "ghc-pkg" srcHash $ \wrapperFp -> writeFile wrapperFp contents

    -- Escape the filepath and trim excess newlines added by 'escapeArgs'
    escapeFilePath fp = trimEnd $ escapeArgs [fp]

-- | @'cabalCradleDependencies' projectFile rootDir componentDir@.
-- Compute the dependencies of the cabal cradle based
-- on cabal project configuration, the cradle root and the component directory.
--
-- The @projectFile@ and @projectFile <> ".local"@ are always added to the list
-- of dependencies.
--
-- Directory 'componentDir' is a sub-directory where we look for
-- package specific cradle dependencies, such as a '.cabal' file.
--
-- Found dependencies are relative to 'rootDir'.
cabalCradleDependencies :: CradleProjectConfig -> FilePath -> FilePath -> IO [FilePath]
cabalCradleDependencies projectFile rootDir componentDir = do
    let relFp = makeRelative rootDir componentDir
    cabalFiles' <- findCabalFiles componentDir
    let cabalFiles = map (relFp </>) cabalFiles'
    return $ map normalise $ cabalFiles ++ projectLocationOrDefault projectFile

-- |Find .cabal files in the given directory.
--
-- Might return multiple results,biosAction as we can not know in advance
-- which one is important to the user.
findCabalFiles :: FilePath -> IO [FilePath]
findCabalFiles wdir = do
  dirContent <- listDirectory wdir
  return $ filter ((== ".cabal") . takeExtension) dirContent


processCabalWrapperArgs :: [String] -> Maybe (FilePath, [String])
processCabalWrapperArgs args =
    case args of
        (dir: ghc_args) ->
            let final_args =
                    removeVerbosityOpts
                    $ removeRTS
                    $ removeInteractive ghc_args
            in Just (dir, final_args)
        _ -> Nothing

-- | GHC process information.
-- Consists of the filepath to the ghc executable and
-- arguments to the executable.
type GhcProc = (FilePath, [String])

-- | Generate a fake GHC that can be passed to cabal or stack
-- when run with --interactive, it will print out its
-- command-line arguments and exit
withGhcWrapperTool :: LogAction IO (WithSeverity Log) -> GhcProc -> FilePath -> IO FilePath
withGhcWrapperTool l (mbGhc, ghcArgs) wdir = do
    let wrapperContents = if isWindows then cabalWrapperHs else cabalWrapper
        withExtension fp = if isWindows then fp <.> "exe" else fp
        srcHash = show (fingerprintString wrapperContents)
    cacheFile (withExtension "wrapper") srcHash $ \wrapper_fp ->
      if isWindows
      then
        withSystemTempDirectory "hie-bios" $ \ tmpDir -> do
          let wrapper_hs = wrapper_fp -<.> "hs"
          writeFile wrapper_hs wrapperContents
          let ghcArgsWithExtras = ghcArgs ++ ["-rtsopts=ignore", "-outputdir", tmpDir, "-o", wrapper_fp, wrapper_hs]
          let ghcProc = (proc mbGhc ghcArgsWithExtras)
                      { cwd = Just wdir
                      }
          l <& LogCreateProcessRun ghcProc `WithSeverity` Debug
          readCreateProcess ghcProc "" >>= putStr
      else writeFile wrapper_fp wrapperContents

-- | Create and cache a file in hie-bios's cache directory.
--
-- @'cacheFile' fpName srcHash populate@. 'fpName' is the pattern name of the
-- cached file you want to create. 'srcHash' is the hash that is appended to
-- the file pattern and is expected to change whenever you want to invalidate
-- the cache.
--
-- If the cached file's 'srcHash' changes, then a new file is created, but
-- the old cached file name will not be deleted.
--
-- If the file does not exist yet, 'populate' is invoked with cached file
-- location and it is expected that the caller persists the given filepath in
-- the File System.
cacheFile :: FilePath -> String -> (FilePath -> IO ()) -> IO FilePath
cacheFile fpName srcHash populate = do
  cacheDir <- getCacheDir ""
  createDirectoryIfMissing True cacheDir
  let newFpName = cacheDir </> (dropExtensions fpName <> "-" <> srcHash) <.> takeExtensions fpName
  unlessM (doesFileExist newFpName) $ do
    populate newFpName
    setMode newFpName
  pure newFpName
  where
    setMode wrapper_fp = setFileMode wrapper_fp accessModes

-- | Given the root directory, get the build dir we are using for cabal
-- In the `hie-bios` cache directory
cabalBuildDir :: FilePath -> IO FilePath
cabalBuildDir workDir = do
  abs_work_dir <- makeAbsolute workDir
  let dirHash = show (fingerprintString abs_work_dir)
  getCacheDir ("dist-" <> filter (not . isSpace) (takeBaseName abs_work_dir)<>"-"<>dirHash)

-- | Discover the location of the ghc binary 'cabal' is going to use together
-- with its libdir location.
-- The ghc executable is an absolute path, but not necessarily canonicalised
-- or normalised. Additionally, the ghc path returned is likely to be the raw
-- executable, i.e. without the usual wrapper shims on non-windows systems.
-- If you want to use the given ghc executable, you should invoke
-- 'withGhcWrapperTool'.
--
-- If cabal can not figure it out, a 'CradleError' is returned.
cabalGhcDirs :: LogAction IO (WithSeverity Log) -> CradleProjectConfig -> FilePath -> CradleLoadResultT IO (FilePath, FilePath)
cabalGhcDirs l cabalProject workDir = do
  libdir <- readProcessWithCwd_ l workDir "cabal"
      (["exec"] ++
       projectFileArgs ++
       ["-v0", "--", "ghc", "--print-libdir"]
      )
      ""
  exe <- readProcessWithCwd_ l workDir "cabal"
      -- DON'T TOUCH THIS CODE
      -- This works with 'NoImplicitPrelude', with 'RebindableSyntax' and other shenanigans.
      -- @-package-env=-@ doesn't work with ghc prior 8.4.x
      ([ "exec"] ++
       projectFileArgs ++
       [ "-v0", "--" , "ghc", "-package-env=-", "-ignore-dot-ghci", "-e"
       , "Control.Monad.join (Control.Monad.fmap System.IO.putStr System.Environment.getExecutablePath)"
       ]
      )
      ""
  pure (trimEnd exe, trimEnd libdir)
  where
    projectFileArgs = projectFileProcessArgs cabalProject

cabalAction
  :: ResolvedCradles a
  -> FilePath
  -> Maybe String
  -> LogAction IO (WithSeverity Log)
  -> CradleProjectConfig
  -> FilePath
  -> LoadStyle
  -> CradleLoadResultT IO ComponentOptions
cabalAction (ResolvedCradles root cs vs) workDir mc l projectFile fp loadStyle = do
  cabal_version <- liftIO $ runCachedIO $ cabalVersion vs
  ghc_version   <- liftIO $ runCachedIO $ ghcVersion vs
  -- determine which load style is supported by this cabal cradle.
  determinedLoadStyle <- case (cabal_version, ghc_version) of
    (Just cabal, Just ghc)
      -- Multi-component supported from cabal-install 3.11
      -- and ghc 9.4
      | LoadWithContext _ <- loadStyle ->
          if ghc >= makeVersion [9,4] && cabal >= makeVersion [3,11]
            then pure loadStyle
            else do
              liftIO $ l <& WithSeverity
                (LogLoadWithContextUnsupported "cabal"
                  $ Just "cabal or ghc version is too old. We require `cabal >= 3.11` and `ghc >= 9.4`"
                )
                Warning
              pure LoadFile
    _ -> pure LoadFile

  let cabalArgs = case determinedLoadStyle of
        LoadFile -> [fromMaybe (fixTargetPath fp) mc]
        LoadWithContext fps -> concat
          [ [ "--keep-temp-files"
            , "--enable-multi-repl"
            , fromMaybe (fixTargetPath fp) mc
            ]
          , [fromMaybe (fixTargetPath old_fp) old_mc
            | old_fp <- fps
            -- Lookup the component for the old file
            , Just (ResolvedCradle{concreteCradle = ConcreteCabal ct}) <- [selectCradle prefix old_fp cs]
            -- Only include this file if the old component is in the same project
            , (projectConfigFromMaybe root (cabalProjectFile ct)) == projectFile
            , let old_mc = cabalComponent ct
            ]
          ]

  liftIO $ l <& LogComputedCradleLoadStyle "cabal" determinedLoadStyle `WithSeverity` Info

  let
    cabalCommand = "v2-repl"

  cabalProc <- cabalProcess l projectFile workDir cabalCommand cabalArgs `modCradleError` \err -> do
      deps <- cabalCradleDependencies projectFile workDir workDir
      pure $ err { cradleErrorDependencies = cradleErrorDependencies err ++ deps }

  (ex, output, stde, [(_, maybeArgs)]) <- liftIO $ readProcessWithOutputs [hie_bios_output] l workDir cabalProc
  let args = fromMaybe [] maybeArgs

  let errorDetails =
        ["Failed command: " <> prettyCmdSpec (cmdspec cabalProc)
        , unlines output
        , unlines stde
        , unlines $ args
        , "Process Environment:"]
        <> prettyProcessEnv cabalProc

  when (ex /= ExitSuccess) $ do
    deps <- liftIO $ cabalCradleDependencies projectFile workDir workDir
    let cmd = show (["cabal", cabalCommand] <> cabalArgs)
    let errorMsg = "Failed to run " <> cmd <> " in directory \"" <> workDir <> "\". Consult the logs for full command and error."
    throwCE (CradleError deps ex ([errorMsg] <> errorDetails))

  case processCabalWrapperArgs args of
    Nothing -> do
      -- Provide some dependencies an IDE can look for to trigger a reload.
      -- Best effort. Assume the working directory is the
      -- root of the component, so we are right in trivial cases at least.
      deps <- liftIO $ cabalCradleDependencies projectFile workDir workDir
      throwCE (CradleError deps ex $ ["Failed to parse result of calling cabal" ] <> errorDetails)
    Just (componentDir, final_args) -> do
      deps <- liftIO $ cabalCradleDependencies projectFile workDir componentDir
      CradleLoadResultT $ pure $ makeCradleResult (ex, stde, componentDir, final_args) deps
  where
    -- Need to make relative on Windows, due to a Cabal bug with how it
    -- parses file targets with a C: drive in it
    fixTargetPath x
      | isWindows && hasDrive x = makeRelative workDir x
      | otherwise = x

removeInteractive :: [String] -> [String]
removeInteractive = filter (/= "--interactive")

-- | Strip out any ["+RTS", ..., "-RTS"] sequences in the command string list.
data InRTS = OutsideRTS | InsideRTS

-- | Strip out any ["+RTS", ..., "-RTS"] sequences in the command string list.
--
-- >>> removeRTS ["option1", "+RTS -H32m -RTS", "option2"]
-- ["option1", "option2"]
--
-- >>> removeRTS ["option1", "+RTS", "-H32m", "-RTS", "option2"]
-- ["option1", "option2"]
--
-- >>> removeRTS ["option1", "+RTS -H32m"]
-- ["option1"]
--
-- >>> removeRTS ["option1", "+RTS -H32m", "-RTS", "option2"]
-- ["option1", "option2"]
--
-- >>> removeRTS ["option1", "+RTS -H32m", "-H32m -RTS", "option2"]
-- ["option1", "option2"]
removeRTS :: [String] -> [String]
removeRTS = go OutsideRTS
  where
    go :: InRTS -> [String] -> [String]
    go _ [] = []
    go OutsideRTS (y:ys)
      | "+RTS" `isPrefixOf` y = go (if "-RTS" `isSuffixOf` y then OutsideRTS else InsideRTS) ys
      | otherwise = y : go OutsideRTS ys
    go InsideRTS (y:ys) = go (if "-RTS" `isSuffixOf` y then OutsideRTS else InsideRTS) ys


removeVerbosityOpts :: [String] -> [String]
removeVerbosityOpts = filter ((&&) <$> (/= "-v0") <*> (/= "-w"))


cabalWorkDir :: FilePath -> MaybeT IO FilePath
cabalWorkDir wdir =
      findFileUpwards (== "cabal.project") wdir
  <|> findFileUpwards (\fp -> takeExtension fp == ".cabal") wdir


------------------------------------------------------------------------

-- | Explicit data-type for project configuration location.
-- It is basically a 'Maybe' type, but helps to document the API
-- and helps to avoid incorrect usage.
data CradleProjectConfig
  = NoExplicitConfig
  | ExplicitConfig FilePath
  deriving Eq

-- | Create an explicit project configuration. Expects a working directory
-- followed by an optional name of the project configuration.
projectConfigFromMaybe :: FilePath -> Maybe FilePath -> CradleProjectConfig
projectConfigFromMaybe _wdir Nothing = NoExplicitConfig
projectConfigFromMaybe wdir (Just fp) = ExplicitConfig (wdir </> fp)

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
        _ <- readProcessWithCwd_ l wdir "stack" (stackYamlProcessArgs syaml <> ["setup", "--silent"]) ""
        readProcessWithCwd_ l wdir "stack"
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
stackAction workDir mc syaml l _fp loadStyle = do
  logCradleHasNoSupportForLoadWithContext l loadStyle "stack"
  let ghcProcArgs = ("stack", stackYamlProcessArgs syaml <> ["exec", "ghc", "--"])
  -- Same wrapper works as with cabal
  wrapper_fp <- withGhcWrapperTool l ghcProcArgs workDir
  (ex1, _stdo, stde, [(_, maybeArgs)]) <-
    readProcessWithOutputs [hie_bios_output] l workDir
      $ stackProcess syaml
          $  ["repl", "--no-nix-pure", "--with-ghc", wrapper_fp]
          <> [ comp | Just comp <- [mc] ]

  (ex2, pkg_args, stdr, _) <-
    readProcessWithOutputs [hie_bios_output] l workDir
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
                  (CradleError deps ex1 $
                    [ "Failed to parse result of calling stack" ]
                    ++ stde
                    ++ args
                  )

      Just (componentDir, ghc_args) -> do
        deps <- stackCradleDependencies workDir componentDir syaml
        pure $ makeCradleResult
                  ( combineExitCodes [ex1, ex2]
                  , stde ++ stdr, componentDir
                  , ghc_args ++ pkg_ghc_args
                  )
                  deps

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
stackWorkDir = findFileUpwards isStack
  where
    isStack name = name == "stack.yaml"

{-
-- Support removed for 0.3 but should be added back in the future
----------------------------------------------------------------------------
-- rules_haskell - Thanks for David Smith for helping with this one.
-- Looks for the directory containing a WORKSPACE file
--
rulesHaskellWorkDir :: FilePath -> MaybeT IO FilePath
rulesHaskellWorkDir fp =
  findFileUpwards (== "WORKSPACE") fp

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
------------------------------------------------------------------------------
-- Utilities


-- | Searches upwards for the first directory containing a file to match
-- the predicate.
findFileUpwards :: (FilePath -> Bool) -> FilePath -> MaybeT IO FilePath
findFileUpwards p dir = do
  cnts <-
    liftIO
    $ handleJust
        -- Catch permission errors
        (\(e :: IOError) -> if isPermissionError e then Just [] else Nothing)
        pure
        (findFile p dir)

  case cnts of
    [] | dir' == dir -> fail "No cabal files"
            | otherwise   -> findFileUpwards p dir'
    _ : _ -> return dir
  where dir' = takeDirectory dir

-- | Sees if any file in the directory matches the predicate
findFile :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
findFile p dir = do
  b <- doesDirectoryExist dir
  if b then getFiles >>= filterM doesPredFileExist else return []
  where
    getFiles = filter p <$> getDirectoryContents dir
    doesPredFileExist file = doesFileExist $ dir </> file

-- | Some environments (e.g. stack exec) include GHC_PACKAGE_PATH.
-- Cabal v2 *will* complain, even though or precisely because it ignores them.
-- Unset them from the environment to sidestep this
getCleanEnvironment :: IO [(String, String)]
getCleanEnvironment = do
  Map.toList . Map.delete "GHC_PACKAGE_PATH" . Map.fromList <$> getEnvironment

type Outputs = [OutputName]
type OutputName = String

-- | Call a given process with temp files for the process to write to.
-- * The process can discover the temp files paths by reading the environment.
-- * The contents of the temp files are returned by this function, if any.
-- * The logging function is called every time the process emits anything to stdout or stderr.
-- it can be used to report progress of the process to a user.
-- * The process is executed in the given directory.
readProcessWithOutputs
  :: Outputs  -- ^ Names of the outputs produced by this process
  -> LogAction IO (WithSeverity Log) -- ^ Output of the process is emitted as logs.
  -> FilePath -- ^ Working directory. Process is executed in this directory.
  -> CreateProcess -- ^ Parameters for the process to be executed.
  -> IO (ExitCode, [String], [String], [(OutputName, Maybe [String])])
readProcessWithOutputs outputNames l workDir cp = flip runContT return $ do
  old_env <- liftIO getCleanEnvironment
  output_files <- traverse (withOutput old_env) outputNames

  let process = cp { env = Just $ output_files ++ fromMaybe old_env (env cp),
                     cwd = Just workDir
                    }

    -- Windows line endings are not converted so you have to filter out `'r` characters
  let loggingConduit = C.decodeUtf8  C..| C.lines C..| C.filterE (/= '\r')
        C..| C.map T.unpack C..| C.iterM (\msg -> l <& LogProcessOutput msg `WithSeverity` Debug) C..| C.sinkList
  liftIO $ l <& LogCreateProcessRun process `WithSeverity` Info
  (ex, stdo, stde) <- liftIO $ sourceProcessWithStreams process mempty loggingConduit loggingConduit

  res <- forM output_files $ \(name,path) ->
          liftIO $ (name,) <$> readOutput path

  return (ex, stdo, stde, res)

    where
      readOutput :: FilePath -> IO (Maybe [String])
      readOutput path = do
        haveFile <- doesFileExist path
        if haveFile
          then withFile path ReadMode $ \handle -> do
            hSetBuffering handle LineBuffering
            !res <- force <$> hGetContents handle
            return $ Just $ lines $ filter (/= '\r') res
          else
            return Nothing

      withOutput :: [(String,String)] -> OutputName -> ContT a IO (OutputName, String)
      withOutput env' name =
        case lookup name env' of
          Just file@(_:_) -> ContT $ \action -> do
            removeFileIfExists file
            action (name, file)
          _ -> ContT $ \action -> withSystemTempFile name $ \ file h -> do
            hClose h
            removeFileIfExists file
            action (name, file)

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists f = do
  yes <- doesFileExist f
  when yes (removeFile f)

makeCradleResult :: (ExitCode, [String], FilePath, [String]) -> [FilePath] -> CradleLoadResult ComponentOptions
makeCradleResult (ex, err, componentDir, gopts) deps =
  case ex of
    ExitFailure _ -> CradleFail (CradleError deps ex err)
    _ ->
        let compOpts = ComponentOptions gopts componentDir deps
        in CradleSuccess compOpts

-- | Calls @ghc --print-libdir@, with just whatever's on the PATH.
runGhcCmdOnPath :: LogAction IO (WithSeverity Log) -> FilePath -> [String] -> IO (CradleLoadResult String)
runGhcCmdOnPath l wdir args = readProcessWithCwd l wdir "ghc" args ""
  -- case mResult of
  --   Nothing

-- | Wrapper around 'readCreateProcess' that sets the working directory and
-- clears the environment, suitable for invoking cabal/stack and raw ghc commands.
readProcessWithCwd :: LogAction IO (WithSeverity Log) -> FilePath -> FilePath -> [String] -> String -> IO (CradleLoadResult String)
readProcessWithCwd l dir cmd args stdin = runCradleResultT $ readProcessWithCwd_ l dir cmd args stdin

readProcessWithCwd_ :: LogAction IO (WithSeverity Log) -> FilePath -> FilePath -> [String] -> String -> CradleLoadResultT IO String
readProcessWithCwd_ l dir cmd args stdin = do
  cleanEnv <- liftIO getCleanEnvironment
  let createdProc' = (proc cmd args) { cwd = Just dir, env = Just cleanEnv }
  readProcessWithCwd' l createdProc' stdin

-- | Wrapper around 'readCreateProcessWithExitCode', wrapping the result in
-- a 'CradleLoadResult'. Provides better error messages than raw 'readCreateProcess'.
readProcessWithCwd' :: LogAction IO (WithSeverity Log) -> CreateProcess -> String -> CradleLoadResultT IO String
readProcessWithCwd' l createdProcess stdin = do
  mResult <- liftIO $ optional $ readCreateProcessWithExitCode createdProcess stdin
  liftIO $ l <& LogCreateProcessRun createdProcess `WithSeverity` Debug
  let cmdString = prettyCmdSpec $ cmdspec createdProcess
  case mResult of
    Just (ExitSuccess, stdo, _) -> pure stdo
    Just (exitCode, stdo, stde) -> throwCE $
      CradleError [] exitCode $
        ["Error when calling " <> cmdString, stdo, stde] <> prettyProcessEnv createdProcess
    Nothing -> throwCE $
      CradleError [] ExitSuccess $
        ["Couldn't execute " <> cmdString] <> prettyProcessEnv createdProcess

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
