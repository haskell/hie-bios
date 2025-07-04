{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils (
  -- * Test Environment
  TestM,
  TestEnv,
  TestConfig (..),
  defConfig,

  -- * Run Tests
  runTestEnv,
  runTestEnv',
  runTestEnvLocal,

  -- * Low-level test-env modification helpers
  setCradle,
  unsetCradle,
  setLoadResult,
  unsetLoadResult,
  setLibDirResult,
  setGhcVersionResult,

  -- * Ask for test environment
  askRoot,
  askStep,
  askLogger,
  askCradle,
  askLoadResult,
  askOrLoadLibDir,
  askLibDir,
  askLibDirResult,
  askGhcVersion,
  askGhcVersionResult,

  -- * Test setup helpers
  step,
  normFile,
  relFile,
  findCradleLoc,
  initCradle,
  initImplicitCradle,
  loadComponentOptions,
  loadComponentOptionsMultiStyle,
  loadRuntimeGhcLibDir,
  loadRuntimeGhcVersion,
  inCradleRootDir,
  loadFileGhc,
  isCabalMultipleCompSupported',

  -- * Assertion helpers
  assertCradle,
  assertLibDirVersion,
  assertGhcVersion,
  assertLibDirVersionIs,
  assertGhcVersionIs,
  assertComponentOptions,
  assertCradleError,
  assertLoadSuccess,
  assertLoadFailure,
  assertLoadNone,
  assertCradleLoadSuccess,
  assertCradleLoadError,

  -- * High-level test helpers
  testDirectoryM,
  testImplicitDirectoryM,
  findCradleForModuleM,
) where

import qualified Colog.Core as L
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.List
import Data.Void
import qualified GHC as G
import HIE.Bios.Cradle
import HIE.Bios.Cradle.Cabal (isCabalMultipleCompSupported)
import HIE.Bios.Cradle.ProgramVersions (makeVersions)
import HIE.Bios.Environment
import HIE.Bios.Flags
import HIE.Bios.Ghc.Api
import qualified HIE.Bios.Ghc.Gap as G
import HIE.Bios.Ghc.Load
import HIE.Bios.Types as HIE
import Prettyprinter
import System.Directory
import System.FilePath
import System.IO.Temp
import Test.Tasty.HUnit
import Colog.Core
import qualified Data.Text as Text
import Data.Function ((&))

-- ---------------------------------------------------------------------------
-- Test configuration and information
-- ---------------------------------------------------------------------------

type TestM a = StateT (TestEnv Void) IO a

data TestConfig = TestConfig
  { useTemporaryDirectory :: Bool
  , testProjectRoots :: FilePath
  , testVerbose :: Bool
  }
  deriving (Eq, Show, Ord)

data TestEnv ext = TestEnv
  { testCradleType :: Maybe (Cradle ext)
  , testLoadResult :: Maybe (CradleLoadResult ComponentOptions)
  , testLibDirResult :: Maybe (CradleLoadResult FilePath)
  , testGhcVersionResult :: Maybe (CradleLoadResult String)
  , testRootDir :: FilePath
  , testLogger :: L.LogAction IO (L.WithSeverity HIE.Log)
  }

defConfig :: TestConfig
defConfig = TestConfig True "./tests/projects" False

runTestEnv :: FilePath -> TestM a -> Bool -> IO a
runTestEnv fp act verbose = runTestEnv' defConfig{testVerbose=verbose} fp act

runTestEnvLocal :: FilePath -> TestM a -> Bool -> IO a
runTestEnvLocal fp act verbose = runTestEnv' defConfig{useTemporaryDirectory = False,testVerbose=verbose} fp act

runTestEnv' :: TestConfig -> FilePath -> TestM a -> IO a
runTestEnv' config root act = do
  -- We need to copy over the directory to somewhere outside the source tree
  -- when we test, since the cabal.project/stack.yaml/hie.yaml file in the root
  -- of this repository interferes with the test cradles!
  let wrapper =
        if useTemporaryDirectory config
          then withTempCopy
          else \r cont -> cont r
      mkEnv root' =
        TestEnv
          { testCradleType = Nothing
          , testLoadResult = Nothing
          , testLibDirResult = Nothing
          , testGhcVersionResult = Nothing
          , testRootDir = root'
          , testLogger = init_logger
          }
      realRoot = testProjectRoots config </> root
  wrapper realRoot $ \root' -> flip evalStateT (mkEnv root') $ do
    step $ "Run test in: " <> root'
    act
  where
    init_logger = L.logStringStderr
      & L.cmap printLog
      & L.filterBySeverity (if testVerbose config then Debug else Error) getSeverity
    printLog (L.WithSeverity l sev) = "[" ++ show sev ++ "] " ++ show (pretty l)

-- ---------------------------------------------------------------------------
-- Modification helpers
-- ---------------------------------------------------------------------------

setCradle :: Cradle Void -> TestM ()
setCradle crd = modify' (\env -> env{testCradleType = Just crd})

unsetCradle :: TestM ()
unsetCradle = modify' (\env -> env{testCradleType = Nothing})

setLoadResult :: CradleLoadResult ComponentOptions -> TestM ()
setLoadResult clr = modify' (\env -> env{testLoadResult = Just clr})

unsetLoadResult :: TestM ()
unsetLoadResult = modify' (\env -> env{testLoadResult = Nothing})

setLibDirResult :: CradleLoadResult FilePath -> TestM ()
setLibDirResult libdir = modify' (\env -> env{testLibDirResult = Just libdir})

setGhcVersionResult :: CradleLoadResult String -> TestM ()
setGhcVersionResult ghcVersion = modify' (\env -> env{testGhcVersionResult = Just ghcVersion})

-- ---------------------------------------------------------------------------
-- Access the Test Environment
-- ---------------------------------------------------------------------------

askRoot :: TestM FilePath
askRoot = gets testRootDir

askStep :: TestM (String -> IO ())
askStep = do
  logger <- gets testLogger
  pure $ \s ->
    logger <& LogAny (Text.pack s) `WithSeverity` Info

askCradle :: TestM (Cradle Void)
askCradle =
  gets testCradleType >>= \case
    Just crd -> pure crd
    Nothing ->
      liftIO $
        assertFailure
          "No Cradle set, use 'initCradle' or 'initImplicitCradle' before asking for it"

askLoadResult :: TestM (CradleLoadResult ComponentOptions)
askLoadResult =
  gets testLoadResult >>= \case
    Just crd -> pure crd
    Nothing ->
      liftIO $
        assertFailure
          "No CradleLoadResult set, use 'loadComponent' before asking for it"

askOrLoadLibDir :: TestM FilePath
askOrLoadLibDir =
  gets testLibDirResult >>= \case
    Just lrLibDir ->
      assertCradleLoadSuccess lrLibDir
    Nothing -> do
      loadRuntimeGhcLibDir
      askLibDir

askLibDir :: TestM FilePath
askLibDir = do
  assertCradleLoadSuccess =<< askLibDirResult

askLibDirResult :: TestM (CradleLoadResult FilePath)
askLibDirResult =
  gets testLibDirResult >>= \case
    Just lrLibDir -> pure lrLibDir
    Nothing ->
      liftIO $
        assertFailure
          "No Lib Dir set, use 'loadRuntimeGhcLibDir' before asking for it"

askGhcVersion :: TestM String
askGhcVersion = do
  assertCradleLoadSuccess =<< askGhcVersionResult

askGhcVersionResult :: TestM (CradleLoadResult String)
askGhcVersionResult =
  gets testGhcVersionResult >>= \case
    Just lrGhcVersion -> pure lrGhcVersion
    Nothing ->
      liftIO $
        assertFailure
          "No GHC version set, use 'loadRuntimeGhcVersion' before asking for it"

askLogger :: TestM (L.LogAction IO (L.WithSeverity HIE.Log))
askLogger = gets testLogger

-- ---------------------------------------------------------------------------
-- Test setup helpers
-- ---------------------------------------------------------------------------

step :: String -> TestM ()
step msg = do
  s <- askStep
  liftIO $ s msg

normFile :: FilePath -> TestM FilePath
normFile fp = (</> fp) <$> gets testRootDir

relFile :: FilePath -> TestM FilePath
relFile fp = (`makeRelative` fp) <$> gets testRootDir

findCradleLoc :: FilePath -> TestM (Maybe FilePath)
findCradleLoc fp = do
  a_fp <- normFile fp
  liftIO $ findCradle a_fp

initCradle :: FilePath -> TestM ()
initCradle fp = do
  a_fp <- normFile fp
  step $ "Finding Cradle for: " <> fp
  mcfg <- findCradleLoc a_fp
  relMcfg <- traverse relFile mcfg
  step $ "Loading Cradle: " <> show relMcfg
  logger <- askLogger
  crd <- case mcfg of
    Just cfg -> liftIO $ loadCradle logger cfg
    Nothing -> liftIO $ loadImplicitCradle logger a_fp
  setCradle crd

initImplicitCradle :: FilePath -> TestM ()
initImplicitCradle fp = do
  a_fp <- normFile fp
  step $ "Loading implicit Cradle for: " <> fp
  logger <- askLogger
  crd <- liftIO $ loadImplicitCradle logger a_fp
  setCradle crd

loadComponentOptions :: FilePath -> TestM ()
loadComponentOptions fp = do
  a_fp <- normFile fp
  crd <- askCradle
  step $ "Initialise flags for: " <> fp
  clr <- liftIO $ getCompilerOptions a_fp LoadFile crd
  setLoadResult clr

loadComponentOptionsMultiStyle :: FilePath -> [FilePath] -> TestM ()
loadComponentOptionsMultiStyle fp fps = do
  a_fp <- normFile fp
  a_fps <- mapM normFile fps
  crd <- askCradle
  step $ "Initialise flags for: " <> fp <> " and " <> show fps
  clr <- liftIO $ getCompilerOptions a_fp (LoadWithContext a_fps) crd
  setLoadResult clr

loadRuntimeGhcLibDir :: TestM ()
loadRuntimeGhcLibDir = do
  crd <- askCradle
  step "Load run-time ghc libdir"
  libdirRes <- liftIO $ getRuntimeGhcLibDir crd
  setLibDirResult libdirRes

loadRuntimeGhcVersion :: TestM ()
loadRuntimeGhcVersion = do
  crd <- askCradle
  step "Load run-time ghc version"
  ghcVersionRes <- liftIO $ getRuntimeGhcVersion crd
  setGhcVersionResult ghcVersionRes

isCabalMultipleCompSupported' :: TestM Bool
isCabalMultipleCompSupported' = do
  cr <- askCradle
  root <- askRoot
  versions <- liftIO $ makeVersions (cradleLogger cr) root ((runGhcCmd . cradleOptsProg) cr)
  liftIO $ isCabalMultipleCompSupported versions

inCradleRootDir :: TestM a -> TestM a
inCradleRootDir act = do
  crd <- askCradle
  prev <- liftIO getCurrentDirectory
  liftIO $ setCurrentDirectory (cradleRootDir crd)
  a <- act
  liftIO $ setCurrentDirectory prev
  pure a

loadFileGhc :: FilePath -> TestM ()
loadFileGhc fp = do
  libdir <- askOrLoadLibDir
  a_fp <- normFile fp
  stepF <- askStep
  step "Cradle load"
  loadComponentOptions fp
  opts <- assertLoadSuccess
  liftIO $
    G.runGhc (Just libdir) $ do
      let (ini, _) = initSessionWithMessage (Just G.batchMsg) opts
      sf <- ini
      case sf of
        -- Test resetting the targets
        Succeeded -> do
          liftIO $ stepF "Set target files"
          setTargetFiles mempty [(a_fp, a_fp)]
        Failed -> liftIO $ assertFailure "Module loading failed"

-- ---------------------------------------------------------------------------
-- Assertion helpers for hie-bios
-- ---------------------------------------------------------------------------

assertCradle :: (Cradle Void -> Bool) -> TestM ()
assertCradle cradlePred = do
  crd <- askCradle
  liftIO $ cradlePred crd @? "Must be the correct kind of cradle, got " ++ show (actionName $ cradleOptsProg crd)

assertLibDirVersion :: TestM ()
assertLibDirVersion = assertLibDirVersionIs VERSION_ghc

assertGhcVersion :: TestM ()
assertGhcVersion = assertGhcVersionIs VERSION_ghc

assertLibDirVersionIs :: String -> TestM ()
assertLibDirVersionIs ghcVersion = do
  step $ "Verify runtime GHC library directory is: " <> ghcVersion
  libdir <- askLibDir
  liftIO $
    ghcVersion `isInfixOf` libdir @? "Expected \"" <> ghcVersion
      <> "\" to be infix of: "
      <> libdir

assertGhcVersionIs :: String -> TestM ()
assertGhcVersionIs expectedVersion = do
  step $ "Verify runtime GHC version is: " <> expectedVersion
  ghcVersion <- askGhcVersion
  liftIO $ ghcVersion @?= expectedVersion

assertComponentOptions :: (ComponentOptions -> Assertion) -> TestM ()
assertComponentOptions cont = do
  opts <- assertLoadSuccess
  liftIO $ cont opts

assertCradleError :: (CradleError -> Assertion) -> TestM ()
assertCradleError cont = do
  err <- assertLoadFailure
  liftIO $ cont err

assertLoadSuccess :: TestM ComponentOptions
assertLoadSuccess = do
  askLoadResult >>= \case
    CradleSuccess opts -> pure opts
    other -> liftIO $ assertFailure $ "Expected CradleSuccess but got: " <> show other

assertLoadFailure :: TestM CradleError
assertLoadFailure = do
  askLoadResult >>= \case
    CradleFail err -> pure err
    other -> liftIO $ assertFailure $ "Expected CradleFail but got: " <> show other

assertLoadNone :: TestM ()
assertLoadNone = do
  askLoadResult >>= \case
    CradleNone -> pure ()
    other -> liftIO $ assertFailure $ "Expected CradleNone but got: " <> show other

assertCradleLoadSuccess :: CradleLoadResult a -> TestM a
assertCradleLoadSuccess = \case
  (CradleSuccess x) -> pure x
  CradleNone -> liftIO $ assertFailure "Unexpected none-Cradle"
  (CradleFail (CradleError _deps _ex stde _err_loading_files)) ->
    liftIO $ assertFailure ("Unexpected cradle fail" <> unlines stde)

assertCradleLoadError :: CradleLoadResult a -> TestM CradleError
assertCradleLoadError = \case
  (CradleSuccess _) -> liftIO $ assertFailure "Unexpected CradleSuccess"
  CradleNone -> liftIO $ assertFailure "Unexpected none-Cradle"
  (CradleFail err) -> pure err

-- ---------------------------------------------------------------------------
-- High-level, re-usable assertions
-- ---------------------------------------------------------------------------

testDirectoryM :: (Cradle Void -> Bool) -> FilePath -> TestM ()
testDirectoryM cradlePred file = do
  initCradle file
  assertCradle cradlePred
  loadRuntimeGhcLibDir
  assertLibDirVersion
  loadRuntimeGhcVersion
  assertGhcVersion
  loadFileGhc file

testImplicitDirectoryM :: (Cradle Void -> Bool) -> FilePath -> TestM ()
testImplicitDirectoryM cradlePred file = do
  initImplicitCradle file
  assertCradle cradlePred
  loadRuntimeGhcLibDir
  assertLibDirVersion
  loadRuntimeGhcVersion
  assertGhcVersion
  loadFileGhc file

findCradleForModuleM :: FilePath -> Maybe FilePath -> TestM ()
findCradleForModuleM fp expected' = do
  rootFp <- askRoot
  let expected = fmap (rootFp </>) expected'
  crd <- findCradleLoc fp
  liftIO $ crd @?= expected

-- ---------------------------------------------------------------------------
-- Copy Directory system utilities
-- ---------------------------------------------------------------------------

withTempCopy :: FilePath -> (FilePath -> IO a) -> IO a
withTempCopy srcDir f =
  withSystemTempDirectory "hie-bios-test" $ \newDir -> do
    exists <- doesDirectoryExist srcDir
    when exists $ do
      copyDir srcDir newDir
    f newDir

copyDir :: FilePath -> FilePath -> IO ()
copyDir src dst = do
  contents <- listDirectory src
  forM_ contents $ \file -> do
    unless (file `elem` ignored) $ do
      let srcFp = src </> file
          dstFp = dst </> file
      isDir <- doesDirectoryExist srcFp
      if isDir
        then createDirectory dstFp >> copyDir srcFp dstFp
        else copyFile srcFp dstFp
 where
  ignored = ["dist", "dist-newstyle", ".stack-work"]
