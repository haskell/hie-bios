{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module HIE.Bios.Cradle.Cabal
  (
  -- * Cabal Cradle interface
  cabalAction,
  runCabalGhcCmd,
  -- * Locations
  findCabalFiles,
  -- * Wrappers
  withGhcWrapperTool,
  -- * Argument processing
  processCabalWrapperArgs,
  -- * Exposed for tests
  isCabalMultipleCompSupported,
  )where

import Data.Char (isSpace)
import System.Exit
import System.Directory
import Colog.Core (LogAction (..), WithSeverity (..), Severity (..), (<&))
import Control.Monad
import Control.Monad.Extra (concatMapM)
import Control.Monad.IO.Class
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import Data.Conduit.Process
import Data.Maybe (fromMaybe)
import Data.List
import Data.List.Extra (trimEnd, nubOrd)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.FilePath
import System.Info.Extra (isWindows)
import System.IO.Temp
import Data.Version

import HIE.Bios.Config
import HIE.Bios.Environment (getCacheDir)
import HIE.Bios.Types hiding (ActionName(..))
import HIE.Bios.Wrappers
import qualified HIE.Bios.Process as Process
import HIE.Bios.Cradle.ProjectConfig
import HIE.Bios.Cradle.Utils
import HIE.Bios.Cradle.ProgramVersions
import HIE.Bios.Cradle.Resolved
import HIE.Bios.Process

import GHC.Fingerprint (fingerprintString)
import GHC.ResponseFile (escapeArgs)

{- Note [Finding ghc-options with cabal]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to know how to compile a cabal component with GHC.
There are two main ways to obtain the ghc-options:

1. `cabal --with-ghc <ghc-shim>` (for exe:cabal <3.15 or lib:Cabal <3.15)

In this approach, we generate a <ghc-shim> which is passed to the exe:cabal process.
If a package needs to be compiled, we compile the package with the same GHC process that
exe:cabal would have used.

If the first argument is `--interactive`, then we do not launch the GHCi process,
but record all the arguments for later processing.

2. `cabal --with-repl <repl-shim>` (for exe:cabal >=3.15 and lib:Cabal >=3.15)

The <repl-shim> is notably simpler than the <ghc-shim>, as `--with-repl` invokes
<repl-shim> *only* as the final GHCi process, not for compiling dependencies or
executing preprocessors.

Thus, <repl-shim> merely needs to log all arguments that are passed to <repl-shim>.

This is the simpler, more maintainable approach, with fewer unintended side-effects.

=== Finding the GHC process cabal uses to compile a project with

We want HLS and hie-bios to honour the `with-compiler` field in `cabal.project` files.
Again, we identify two ways to find the exact GHC program that is going to be invoked by cabal.

1. `cabal exec -- ghc --interactive -e System.Environment.getExecutablePath` (for exe:cabal <3.14)

Ignoring a couple of details, we can get the path to the raw executable by asking
the GHCi process for its executable path.
The issue is that on linux, the executable path is insufficient, the GHC executable
invoked by the user is "wrapped" in a shim that specifies the libdir location, e.g.:

    > cat /home/hugin/.ghcup/bin/ghc
    #!/bin/sh
    exedir="/home/hugin/.ghcup/ghc/9.6.7/lib/ghc-9.6.7/bin"
    exeprog="./ghc-9.6.7"
    executablename="/home/hugin/.ghcup/ghc/9.6.7/lib/ghc-9.6.7/bin/./ghc-9.6.7"
    bindir="/home/hugin/.ghcup/ghc/9.6.7/bin"
    libdir="/home/hugin/.ghcup/ghc/9.6.7/lib/ghc-9.6.7/lib"
    docdir="/home/hugin/.ghcup/ghc/9.6.7/share/doc/ghc-9.6.7"
    includedir="/home/hugin/.ghcup/ghc/9.6.7/include"

    exec "$executablename" -B"$libdir" ${1+"$@"}

We find the libdir by asking GHC via `cabal exec -- ghc --print-libdir`.
Once we have these two paths, we also need to find the `ghc-pkg` location,
otherwise cabal will use the `ghc-pkg` that is found on PATH, which is not correct
if the user overwrites the compiler field via `with-compiler`.

To find `ghc-pkg`, we assume it is going to be located next to the `libdir`, and then
reconstruct the wrapper shim for `ghc-pkg`.

Then we reconstructed both the ghc and ghc-pkg program that is going to be used by cabal
and can use it in the <ghc-shim> and `cabal repl --with-compiler <ghc-shim> --with-hc-pkg <hc-pkg-shim>`.

Calling `cabal exec` can be very slow on a large codebase, over 1 second per invocation.

2. `cabal path` (for exe:cabal >= 3.14)

We can skip the reconstruction of the GHC shim by using the output of `cabal path --compiler-info`.
This gives us the location of the GHC executable shim, so we don't need to reconstruct any shims.

However, we still have to reconstruct the ghc-pkg shim when using `cabal repl --with-compiler`.

`cabal path` is incredibly fast to invoke, as it circumvents running the cabal solver.
It is easier to maintain as well.
-}

-- | Main entry point into the cabal cradle invocation.
--
-- This function does a lot of work, supporting multiple cabal-install versions and
-- different ways of obtaining the component options.
--
-- See Note [Finding ghc-options with cabal] for a detailed elaboration.
cabalAction ::
  ResolvedCradles a ->
  FilePath ->
  Maybe String ->
  LogAction IO (WithSeverity Log) ->
  CradleProjectConfig ->
  FilePath ->
  LoadStyle ->
  CradleLoadResultT IO ComponentOptions
cabalAction cradles workDir mc l projectFile fp loadStyle = do
  let progVersions = cradleProgramVersions cradles
  multiCompSupport <- isCabalMultipleCompSupported progVersions
  -- determine which load style is supported by this cabal cradle.
  determinedLoadStyle <- case loadStyle of
    LoadWithContext _ | not multiCompSupport -> do
      liftIO $
        l
          <& WithSeverity
            ( LogLoadWithContextUnsupported "cabal" $
                Just "cabal or ghc version is too old. We require `cabal >= 3.11` and `ghc >= 9.4`"
            )
            Warning
      pure LoadFile
    _ -> pure loadStyle

  (cabalArgs, loadingFiles, extraDeps) <- processCabalLoadStyle l cradles projectFile workDir mc fp determinedLoadStyle

  cabalFeatures <- determineCabalLoadFeature progVersions
  let
    -- Used for @cabal >= 3.15@ but @lib:Cabal <3.15@, in custom setups.
    mkFallbackCabalProc = cabalLoadFilesBefore315 l progVersions projectFile workDir cabalArgs
  cabalProc <- case cabalFeatures of
    CabalWithRepl -> cabalLoadFilesWithRepl l projectFile workDir cabalArgs
    CabalWithGhcShimWrapper -> cabalLoadFilesBefore315 l progVersions projectFile workDir cabalArgs

  mResult <- runCabalToGetGhcOptions cabalProc mkFallbackCabalProc
  case mResult of
    Left (code, errorDetails) -> do
      -- Provide some dependencies an IDE can look for to trigger a reload.
      -- Best effort. Assume the working directory is the
      -- root of the component, so we are right in trivial cases at least.
      deps <- liftIO $ cabalCradleDependencies projectFile workDir workDir
      let cmd = prettyCmdSpec (cmdspec cabalProc)
      let errorMsg = "Failed to run " <> cmd <> " in directory \"" <> workDir <> "\". Consult the logs for full command and error."
      throwCE CradleError
        { cradleErrorDependencies = nubOrd (deps <> extraDeps)
        , cradleErrorExitCode = ExitFailure code
        , cradleErrorStderr = [errorMsg] <> prettyProcessErrorDetails errorDetails
        , cradleErrorLoadingFiles = loadingFiles
        }
    Right (args, errorDetails) -> do
      case processCabalWrapperArgs args of
        Nothing -> do
          -- Provide some dependencies an IDE can look for to trigger a reload.
          -- Best effort. Assume the working directory is the
          -- root of the component, so we are right in trivial cases at least.
          deps <- liftIO $ cabalCradleDependencies projectFile workDir workDir
          throwCE CradleError
            { cradleErrorDependencies = nubOrd (deps <> extraDeps)
            , cradleErrorExitCode = ExitSuccess
            , cradleErrorStderr = ["Failed to parse result of calling cabal"] <> prettyProcessErrorDetails errorDetails
            , cradleErrorLoadingFiles = loadingFiles
            }
        Just (componentDir, ghc_args) -> do
          deps <- liftIO $ cabalCradleDependencies projectFile workDir componentDir
          usesResponseFiles <- usesResponseFilesForAllGhcOptions progVersions
          final_args <- case usesResponseFiles of
            True -> liftIO $ expandGhcOptionResponseFile ghc_args
            False -> pure ghc_args
          CradleLoadResultT $ pure $ CradleSuccess
            ComponentOptions
              { componentOptions = final_args
              , componentRoot = componentDir
              , componentDependencies = nubOrd (deps <> extraDeps)
              }
  where
    -- | Run the given cabal process to obtain ghc options.
    -- In the special case of 'cabal >= 3.15' but 'lib:Cabal <3.15' (via custom-setups),
    -- we gracefully fall back to the given action to create an alternative cabal process which
    -- we use to find the ghc options.
    runCabalToGetGhcOptions ::
      Process.CreateProcess ->
      CradleLoadResultT IO Process.CreateProcess ->
      CradleLoadResultT IO
        (Either
          (Int, ProcessErrorDetails)
          ([String], ProcessErrorDetails)
        )
    runCabalToGetGhcOptions cabalProc mkFallbackCabalProc = do
      (ex, output, stde, [(_, maybeArgs)]) <- liftIO $ Process.readProcessWithOutputs [hie_bios_output] l workDir cabalProc
      let args = fromMaybe [] maybeArgs
      let errorDetails = ProcessErrorDetails
            { processCmd = cmdspec cabalProc
            , processStdout = output
            , processStderr = stde
            , processGhcOptions = args
            , processHieBiosEnvironment = hieBiosProcessEnv cabalProc
            }
      case ex of
        ExitFailure{} | isCabalLibraryInProjectTooOld stde -> do
          liftIO $ l <& WithSeverity (LogCabalLibraryTooOld stde) Debug
          fallbackCabalProc <- mkFallbackCabalProc
          runCabalToGetGhcOptions fallbackCabalProc mkFallbackCabalProc
        ExitFailure code -> do

          pure $ Left (code, errorDetails)
        ExitSuccess ->
          pure $ Right (args, errorDetails)


runCabalGhcCmd :: ResolvedCradles a -> FilePath -> LogAction IO (WithSeverity Log) -> CradleProjectConfig -> [String] -> IO (CradleLoadResult String)
runCabalGhcCmd cs wdir l projectFile args = runCradleResultT $ do
  let vs = cradleProgramVersions cs
  callCabalPathForCompilerPath l vs wdir projectFile >>= \case
    Just p -> Process.readProcessWithCwd_ l wdir p args ""
    Nothing -> do
      buildDir <- liftIO $ cabalBuildDir wdir
      -- Workaround for a cabal-install bug on 3.0.0.0:
      -- ./dist-newstyle/tmp/environment.-24811: createDirectory: does not exist (No such file or directory)
      liftIO $ createDirectoryIfMissing True (buildDir </> "tmp")
      -- Need to pass -v0 otherwise we get "resolving dependencies..."
      cabalProc <- cabalExecGhc l vs projectFile wdir args
      Process.readProcessWithCwd' l cabalProc ""

processCabalLoadStyle :: MonadIO m => LogAction IO (WithSeverity Log) -> ResolvedCradles a -> CradleProjectConfig -> [Char] -> Maybe FilePath -> [Char] -> LoadStyle -> m ([FilePath], [FilePath], [FilePath])
processCabalLoadStyle l cradles projectFile workDir mc fp loadStyle = do
  let fpModule = fromMaybe (fixTargetPath fp) mc
  (cabalArgs, loadingFiles, extraDeps) <- case loadStyle of
        LoadFile -> pure ([fpModule], [fp], [])
        LoadWithContext fps -> do
          (modPairs, mergedDeps) <- moduleFilesFromSameProject fps
          let allModPairs = nubOrd $ (fpModule, fp) : modPairs
              allModules  = nubOrd $ fmap fst allModPairs
              allFiles    = nubOrd $ fmap snd allModPairs
          pure (["--enable-multi-repl"] ++ allModules, allFiles, mergedDeps)

  liftIO $ l <& LogComputedCradleLoadStyle "cabal" loadStyle `WithSeverity` Info
  liftIO $ l <& LogCabalLoad fp mc (prefix <$> resolvedCradles cradles) loadingFiles `WithSeverity` Debug
  pure (cabalArgs, loadingFiles, extraDeps)
  where
    -- Need to make relative on Windows, due to a Cabal bug with how it
    -- parses file targets with a C: drive in it. So we decide to make
    -- the paths relative to the working directory.
    fixTargetPath x
      | isWindows && hasDrive x = makeRelative workDir x
      | otherwise = x
    -- Return (moduleTarget,file) pairs for each context file, plus a merged dependency list across all of them.
    moduleFilesFromSameProject :: MonadIO m => [FilePath] -> m ([(FilePath, FilePath)], [FilePath])
    moduleFilesFromSameProject fps = do
      -- First, select eligible files and collect their componentDir and YAML deps
      let selected =
            [ (file, depsYaml, prefix rc, cabalComponent ct)
            | file <- fps
            , Just rc@(ResolvedCradle {concreteCradle = ConcreteCabal ct, cradleDeps = depsYaml}) <- [selectCradle prefix file (resolvedCradles cradles)]
            , (projectConfigFromMaybe (cradleRoot cradles) (cabalProjectFile ct)) == projectFile
            ]
      -- Compute dynamic deps
      let compDirs = nubOrd $ takeDirectory fp : [ takeDirectory f | (f, _, _, _) <- selected ]
      dynDeps <- concatMapM (liftIO . cabalCradleDependenciesEnclosing projectFile (cradleRoot cradles)) compDirs
      -- Combine YAML deps with dynamic deps
      let mergedDeps = nubOrd $ dynDeps ++ concat [ depsYaml | (_, depsYaml, _ , _) <- selected ]
      let modPairs = [ (fromMaybe (fixTargetPath file) old_mc, file)
                     | (file, _, _, old_mc) <- selected ]
      pure (modPairs, mergedDeps)

cabalLoadFilesWithRepl :: LogAction IO (WithSeverity Log) -> CradleProjectConfig -> FilePath -> [String] -> CradleLoadResultT IO CreateProcess
cabalLoadFilesWithRepl l projectFile workDir args = do
  let cabalCommand = "v2-repl"

  newEnvironment <- liftIO Process.getCleanEnvironment
  wrapper_fp <- liftIO $ withReplWrapperTool l (proc "ghc") workDir
  pure (proc "cabal" ([cabalCommand, "--keep-temp-files", "--with-repl", wrapper_fp] <> projectFileProcessArgs projectFile <> args))
    { env = Just newEnvironment
    , cwd = Just workDir
    }

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

-- | @'cabalCradleDependenciesEnclosing' projectFile rootDir startDir@.
-- Find cradle dependency files by walking upwards from a starting directory.
--
-- This is similar to 'cabalCradleDependencies', but instead of looking only in a
-- specific component directory, it searches for @.cabal@ files in @startDir@ and
-- its ancestor directories, stopping at (and not traversing above) @rootDir@ or
-- the filesystem root. All discovered @.cabal@ files are returned relative to
-- @rootDir@, along with the cabal project configuration files determined by
-- 'projectLocationOrDefault'.
--
-- Inputs
-- - projectFile: the cradle's cabal project configuration (explicit file or default).
-- - rootDir: absolute cradle root; used as the boundary for the upward search and
--   to relativize returned paths.
-- - startDir: directory from which to begin searching for enclosing @.cabal@ files.
--
-- Output
-- - A list of normalised, relative file paths that should be watched to trigger
--   a reload when changed.
cabalCradleDependenciesEnclosing :: CradleProjectConfig -> FilePath -> FilePath -> IO [FilePath]
cabalCradleDependenciesEnclosing projectFile rootDir fp = do
    cabalFiles' <- findCabalFilesEnclosing fp
    let relCabalFiles = map (makeRelative rootDir) cabalFiles'
    return $ map normalise $ relCabalFiles ++ projectLocationOrDefault projectFile
    where
      -- find the cabal file upwards from fp to rootDir
      findCabalFilesEnclosing :: FilePath -> IO [FilePath]
      findCabalFilesEnclosing dir = do
            cfs <- map (dir </>) <$> findCabalFiles dir
            if not (null cfs)
              then return cfs
              else
                let parentDir = takeDirectory dir
                in if parentDir == dir || length parentDir < length rootDir
                   then return []
                   else findCabalFilesEnclosing parentDir

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

-- ----------------------------------------------------------------------------
-- Legacy cabal commands to obtain ghc-options.
-- These commands are obsolete in the latest cabal version, but we still support
-- them.
-- ----------------------------------------------------------------------------

cabalLoadFilesBefore315 :: LogAction IO (WithSeverity Log) -> ProgramVersions -> CradleProjectConfig -> [Char] -> [String] -> CradleLoadResultT IO CreateProcess
cabalLoadFilesBefore315 l progVersions projectFile workDir args' = do
  let cabalCommand = "v2-repl"
  cabal_version <- liftIO $ runCachedIO $ cabalVersion progVersions

  let args = case cabal_version of
        Just v | v < makeVersion [3,15] -> "--keep-temp-files" : args'
        _ -> args'
  cabalProcess l progVersions projectFile workDir cabalCommand args `modCradleError` \err -> do
    deps <- cabalCradleDependencies projectFile workDir workDir
    pure $ err {cradleErrorDependencies = cradleErrorDependencies err ++ deps}

-- | Execute a cabal process in our custom cache-build directory configured
-- with the custom ghc executable.
-- The created process has its working directory set to the given working directory.
--
-- Invokes the cabal process in the given directory.
-- Finds the appropriate @ghc@ version as a fallback and provides the path
-- to the custom ghc wrapper via 'hie_bios_ghc' environment variable which
-- the custom ghc wrapper may use as a fallback if it can not respond to certain
-- queries, such as ghc version or location of the libdir.
cabalProcess :: LogAction IO (WithSeverity Log) -> ProgramVersions -> CradleProjectConfig -> FilePath -> String -> [String] -> CradleLoadResultT IO CreateProcess
cabalProcess l vs cabalProject workDir command args = do
  ghcDirs@(ghcBin, libdir) <- callCabalPathForCompilerPath l vs workDir cabalProject >>= \case
    Just p -> do
      libdir <- Process.readProcessWithCwd_ l workDir p ["--print-libdir"] ""
      pure (p, trim libdir)
    Nothing -> cabalGhcDirs l cabalProject workDir

  ghcPkgPath <- liftIO $ withGhcPkgTool ghcBin libdir
  newEnvironment <- liftIO $ setupEnvironment ghcDirs
  cabalProc <- liftIO $ setupCabalCommand ghcPkgPath
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
      environment <- Process.getCleanEnvironment
      pure $ processEnvironment ghcDirs ++ environment

    setupCabalCommand :: FilePath -> IO CreateProcess
    setupCabalCommand ghcPkgPath = do
      wrapper_fp <- withGhcWrapperTool l (proc "ghc") workDir
      buildDir <- cabalBuildDir workDir
      let extraCabalArgs =
            [ "--builddir=" <> buildDir
            , command
            , "--with-compiler", wrapper_fp
            , "--with-hc-pkg", ghcPkgPath
            ]
            <> projectFileProcessArgs cabalProject
      pure $ proc "cabal" (extraCabalArgs ++ args)

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
  libdir <- Process.readProcessWithCwd_ l workDir "cabal"
      (["exec"] ++
       projectFileArgs ++
       ["-v0", "--", "ghc", "--print-libdir"]
      )
      ""
  exe <- Process.readProcessWithCwd_ l workDir "cabal"
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
  pure (trim exe, trim libdir)
  where
    projectFileArgs = projectFileProcessArgs cabalProject

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
    else withGhcPkgShim ghcPkgPath
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
    withGhcPkgShim ghcPkg = do
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

-- ----------------------------------------------------------------------------
-- Wrapper Tools
-- ----------------------------------------------------------------------------

-- | GHC process that accepts GHC arguments.
type GhcProc = [String] -> CreateProcess

-- | Generate a fake GHC that can be passed to cabal or stack
-- when run with --interactive, it will print out its
-- command-line arguments and exit
withGhcWrapperTool :: LogAction IO (WithSeverity Log) -> GhcProc -> FilePath -> IO FilePath
withGhcWrapperTool l mkGhcCall wdir = do
  withWrapperTool l mkGhcCall wdir "wrapper" cabalWrapperHs cabalWrapper

-- | Generate a script/binary that can be passed to cabal's '--with-repl'.
-- On windows, this compiles a Haskell file, while on other systems, we persist
-- a haskell source file and ad-hoc compile it with 'GhcProc'.
--
-- 'GhcProc' is unused on other platforms.
--
withReplWrapperTool :: LogAction IO (WithSeverity Log) -> GhcProc -> FilePath -> IO FilePath
withReplWrapperTool l mkGhcCall wdir =
  withWrapperTool l mkGhcCall wdir "repl-wrapper" cabalWithReplWrapperHs cabalWithReplWrapper

withWrapperTool :: LogAction IO (WithSeverity Log) -> GhcProc -> String -> FilePath -> String -> String -> IO FilePath
withWrapperTool l mkGhcCall wdir baseName windowsWrapper unixWrapper = do
  let wrapperContents = if isWindows then windowsWrapper else unixWrapper
      withExtension fp = if isWindows then fp <.> "exe" else fp
      srcHash = show (fingerprintString wrapperContents)
  cacheFile (withExtension baseName) srcHash $ \wrapper_fp ->
    if isWindows
    then
      withSystemTempDirectory "hie-bios" $ \ tmpDir -> do
        let wrapper_hs = wrapper_fp -<.> "hs"
        writeFile wrapper_hs wrapperContents
        let ghcArgs = ["-rtsopts=ignore", "-outputdir", tmpDir, "-o", wrapper_fp, wrapper_hs]
        let ghcProc = (mkGhcCall ghcArgs)
                    { cwd = Just wdir
                    }
        l <& LogCreateProcessRun ghcProc `WithSeverity` Debug
        readCreateProcess ghcProc "" >>= putStr
    else writeFile wrapper_fp wrapperContents

-- ----------------------------------------------------------------------------
-- 'cabal.project' options
-- ----------------------------------------------------------------------------

projectFileProcessArgs :: CradleProjectConfig -> [String]
projectFileProcessArgs (ExplicitConfig prjFile) = ["--project-file", prjFile]
projectFileProcessArgs NoExplicitConfig = []

projectLocationOrDefault :: CradleProjectConfig -> [FilePath]
projectLocationOrDefault = \case
  NoExplicitConfig -> ["cabal.project", "cabal.project.local"]
  (ExplicitConfig prjFile) -> [prjFile, prjFile <.> "local"]

-- ----------------------------------------------------------------------------
-- cabal locations
-- ----------------------------------------------------------------------------

-- | Given the root directory, get the build dir we are using for cabal
-- In the `hie-bios` cache directory
cabalBuildDir :: FilePath -> IO FilePath
cabalBuildDir workDir = do
  abs_work_dir <- makeAbsolute workDir
  let dirHash = show (fingerprintString abs_work_dir)
  getCacheDir ("dist-" <> filter (not . isSpace) (takeBaseName abs_work_dir)<>"-"<>dirHash)

-- |Find .cabal files in the given directory.
--
-- Might return multiple results,biosAction as we can not know in advance
-- which one is important to the user.
findCabalFiles :: FilePath -> IO [FilePath]
findCabalFiles wdir = do
  dirContent <- listDirectory wdir
  return $ filter ((== ".cabal") . takeExtension) dirContent

-- ----------------------------------------------------------------------------
-- cabal process wrappers and helpers
-- ----------------------------------------------------------------------------

cabalExecGhc :: LogAction IO (WithSeverity Log) -> ProgramVersions -> CradleProjectConfig -> FilePath -> [String] -> CradleLoadResultT IO CreateProcess
cabalExecGhc l vs projectFile wdir args = do
  cabalProcess l vs projectFile wdir "v2-exec" $ ["ghc", "-v0", "--"] ++ args

callCabalPathForCompilerPath :: LogAction IO (WithSeverity Log) -> ProgramVersions -> FilePath -> CradleProjectConfig -> CradleLoadResultT IO (Maybe FilePath)
callCabalPathForCompilerPath l vs workDir projectFile = do
  isCabalPathSupported vs >>= \case
    False -> pure Nothing
    True -> do
      let
        args = ["path", "--output-format=json"] <> projectFileProcessArgs projectFile
        bs = BS.fromStrict . T.encodeUtf8 . T.pack
        parse_compiler_path = Aeson.parseEither ((.: "compiler") >=>  (.: "path")) <=< Aeson.eitherDecode

      compiler_info <- Process.readProcessWithCwd_ l workDir "cabal" args ""
      -- Strip any non-JSON noise lines (e.g. darcs warnings) before parsing.
      -- The JSON output always starts with '{'.
      let strip_noise = dropWhile (\c -> c /= '{') compiler_info
      case parse_compiler_path (bs strip_noise) of
        Left err -> do
          liftIO $ l <& WithSeverity (LogCabalPath $ T.pack err) Warning
          pure Nothing
        Right a -> pure a

-- ----------------------------------------------------------------------------
-- Version and cabal capability checks
-- ----------------------------------------------------------------------------

data CabalLoadFeature
  = CabalWithRepl
  | CabalWithGhcShimWrapper

determineCabalLoadFeature :: MonadIO m => ProgramVersions -> m CabalLoadFeature
determineCabalLoadFeature vs = do
  cabal_version <- liftIO $ runCachedIO $ cabalVersion vs
  -- determine which load style is supported by this cabal cradle.
  case cabal_version of
    Just ver
      | ver >= makeVersion [3, 15] -> pure CabalWithRepl
      | otherwise -> pure CabalWithGhcShimWrapper
    _ -> pure CabalWithGhcShimWrapper

-- | As `cabal repl` started to hit maximum cli invocation length, we changed how repl arguments are
-- passed to GHC. In `cabal 3.15`, `cabal 3.16.0.0`, `cabal 3.17` and onwards, ghc options are
-- passed to GHC via response files.
--
-- This breaks HLS release binary distributions before 2.12, as neither HLS nor hie-bios are capable of handling
-- response files at the top-level.
--
-- In particular, `cabal 3.16.0.0` was released with this change and no HLS bindist before 2.12 works
-- with `cabal 3.16.0.0`.
-- To make `cabal 3.16.*` series compatible with released HLS binaries, we reverted the response file
-- change for the ghc options. This change will apply once `cabal 3.16.1.*` is released.
-- Note, in `cabal 3.17`, i.e. cabal HEAD, we still pass the arguments via response files and will do that for the
-- `cabal 3.18` release.
--
-- So, we have a weird matrix now, between some commit in `cabal 3.15` and `cabal 3.16`, ghc arguments
-- are supplied encoded in a response file, while in `>= cabal 3.16.1`, the arguments are passed verbatim.
-- Then, later on in `cabal-3.17`, we use response files again.
--
-- 'usesResponseFilesForAllGhcOptions' encodes all of this history.
usesResponseFilesForAllGhcOptions :: MonadIO m => ProgramVersions -> m Bool
usesResponseFilesForAllGhcOptions vs = do
  cabal_version <- liftIO $ runCachedIO $ cabalVersion vs
  -- determine which load style is supported by this cabal cradle.
  case cabal_version of
    Just ver
      | ver >= makeVersion [3, 15] && ver <= makeVersion [3, 16, 0, 0] -> pure True
      | ver >= makeVersion [3, 17] -> pure True
      | otherwise -> pure False
    _ -> pure False


-- | When @cabal repl --with-repl@ is called in a project with a custom setup which forces
-- an older @lib:Cabal@ version, then the error message looks roughly like:
--
-- @
--   Error: [Cabal-7107]
--   Could not resolve dependencies:
--   [__0] trying: cabal-with-custom-setup-0.1.0.0 (user goal)
--   [__1] next goal: cabal-with-custom-setup:setup.Cabal (dependency of cabal-with-custom-setup)
--   [__1] rejecting: cabal-with-custom-setup:setup.Cabal; 3.10.3.0/installed-3.10.3.0, ... (constraint from --with-repl requires >=3.15)
--   ...
-- @
--
-- We do a quick and dirty string comparison to check whether the error message looks like it has been caused
-- by using a @lib:Cabal@ version that doesn't support the @--with-repl@ flag.
isCabalLibraryInProjectTooOld :: [String] -> Bool
isCabalLibraryInProjectTooOld stderr =
  "constraint from --with-repl requires >=3.15" `isInfixOf` unlines stderr

isCabalPathSupported :: MonadIO m => ProgramVersions -> m Bool
isCabalPathSupported vs = do
  v <- liftIO $ runCachedIO $ cabalVersion vs
  pure $ maybe False (>= makeVersion [3,14]) v

isCabalMultipleCompSupported :: MonadIO m => ProgramVersions -> m Bool
isCabalMultipleCompSupported vs = do
  cabal_version <- liftIO $ runCachedIO $ cabalVersion vs
  ghc_version <- liftIO $ runCachedIO $ ghcVersion vs
  -- determine which load style is supported by this cabal cradle.
  case (cabal_version, ghc_version) of
    (Just cabal, Just ghc) -> pure $ ghc >= makeVersion [9, 4] && cabal >= makeVersion [3, 11]
    _ -> pure False
