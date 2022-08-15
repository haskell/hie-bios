{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.List
import Data.Void
import qualified GHC as G
import HIE.Bios.Cradle
import HIE.Bios.Environment
import HIE.Bios.Flags
import HIE.Bios.Ghc.Api
import qualified HIE.Bios.Ghc.Gap as G
import HIE.Bios.Ghc.Load
import HIE.Bios.Types
import System.Directory
import System.FilePath
import System.IO.Temp
import Test.Tasty.HUnit

-- ---------------------------------------------------------------------------
-- Test configuration and information
-- ---------------------------------------------------------------------------

type TestM a = StateT (TestEnv Void) IO a

data TestConfig = TestConfig
  { useTemporaryDirectory :: Bool
  }
  deriving (Eq, Show, Ord)

data TestEnv ext = TestEnv
  { testCradleType :: Maybe (Cradle ext)
  , testLoadResult :: Maybe (CradleLoadResult ComponentOptions)
  , testLibDirResult :: Maybe (CradleLoadResult FilePath)
  , testGhcVersionResult :: Maybe (CradleLoadResult String)
  , testRootDir :: FilePath
  , testStepFunction :: String -> IO ()
  }

defConfig :: TestConfig
defConfig = TestConfig True

runTestEnv :: FilePath -> TestM a -> (String -> IO ()) -> IO a
runTestEnv = runTestEnv' defConfig

runTestEnvLocal :: FilePath -> TestM a -> (String -> IO ()) -> IO a
runTestEnvLocal = runTestEnv' defConfig{useTemporaryDirectory = False}

runTestEnv' :: TestConfig -> FilePath -> TestM a -> (String -> IO ()) -> IO a
runTestEnv' config root act stepF = do
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
          , testStepFunction = stepF
          }
  wrapper root (\root' -> evalStateT act (mkEnv root'))

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
askStep = gets testStepFunction

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

-- ---------------------------------------------------------------------------
-- Test setup helpers
-- ---------------------------------------------------------------------------

step :: String -> TestM ()
step msg = do
  s <- gets testStepFunction
  liftIO $ s msg

normFile :: FilePath -> TestM FilePath
normFile fp = (</> fp) <$> gets testRootDir

findCradleLoc :: FilePath -> TestM (Maybe FilePath)
findCradleLoc fp = do
  a_fp <- normFile fp
  liftIO $ findCradle a_fp

initCradle :: FilePath -> TestM ()
initCradle fp = do
  a_fp <- normFile fp
  step $ "Finding Cradle for: " ++ a_fp
  mcfg <- findCradleLoc a_fp
  step $ "Loading Cradle: " ++ show mcfg
  crd <- case mcfg of
    Just cfg -> liftIO $ loadCradle cfg
    Nothing -> liftIO $ loadImplicitCradle a_fp
  setCradle crd

initImplicitCradle :: FilePath -> TestM ()
initImplicitCradle fp = do
  a_fp <- normFile fp
  step $ "Loading implicit Cradle for: " <> fp
  crd <- liftIO $ loadImplicitCradle a_fp
  setCradle crd

loadComponentOptions :: FilePath -> TestM ()
loadComponentOptions fp = do
  a_fp <- normFile fp
  crd <- askCradle
  step $ "Initialise flags for: " <> fp
  clr <- liftIO $ getCompilerOptions mempty a_fp crd
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
  liftIO $ cradlePred crd @? "Must be the correct kind of cradle"

assertLibDirVersion :: TestM ()
assertLibDirVersion = assertLibDirVersionIs VERSION_ghc

assertGhcVersion :: TestM ()
assertGhcVersion = assertGhcVersionIs VERSION_ghc

assertLibDirVersionIs :: String -> TestM ()
assertLibDirVersionIs ghcVersion = do
  step $ "Verify runtime GHC library directory is: " ++ ghcVersion
  libdir <- askLibDir
  liftIO $ ghcVersion `isInfixOf` libdir @? "Expected \"" ++ ghcVersion
      ++ "\" to be infix of: "
      ++ libdir

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
  (CradleFail (CradleError _deps _ex stde)) ->
    liftIO $ assertFailure ("Unexpected cradle fail" ++ unlines stde)

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

-- ---------------------------------------------------------------------------
-- Old test-helpers. To be removed
-- ---------------------------------------------------------------------------

testDirectory :: (Cradle Void -> Bool) -> FilePath -> FilePath -> (String -> IO ()) -> IO ()
testDirectory cradlePred rootDir file step =
  -- We need to copy over the directory to somewhere outside the source tree
  -- when we test, since the cabal.project/stack.yaml/hie.yaml file in the root
  -- of this repository interferes with the test cradles!
  withTempCopy rootDir $ \rootDir' -> do
    fp <- canonicalizePath (rootDir' </> file)
    crd <- initialiseCradle cradlePred fp step
    step "Get runtime GHC library directory"
    testGetGhcLibDir crd VERSION_ghc
    step "Get runtime GHC version"
    testGetGhcVersion crd VERSION_ghc
    step "Initialise Flags"
    testLoadFile crd fp step

{- | Here we are testing that the cradle's method of obtaining the ghcLibDir
 always works.
-}
testGetGhcLibDir :: Cradle a -> String -> IO ()
testGetGhcLibDir crd ghcVersion = do
  libDirRes <- getRuntimeGhcLibDir crd
  isSuccess libDirRes
 where
  -- heuristically test that the produced $libdir makes sense.
  isSuccess (CradleSuccess path) =
    ghcVersion `isInfixOf` path @? "Expected \"" ++ ghcVersion
      ++ "\" to be infix of: "
      ++ path
  isSuccess _ =
    assertFailure "Must succeed loading ghc lib directory"

{- | Here we are testing that the cradle's method of getting the runtime ghc
 version is correct - which while testing, should be the version that we have
 built the tests with. This will fail if you compiled the tests with a ghc
 that doesn't equal the ghc on your path though :(
-}
testGetGhcVersion :: Cradle a -> String -> IO ()
testGetGhcVersion crd ghcVersion = do
  version <- getRuntimeGhcVersion crd
  version @?= CradleSuccess ghcVersion

testGetGhcVersionFail :: (Cradle Void -> Bool) -> FilePath -> FilePath -> (CradleError -> Assertion) -> (String -> IO ()) -> IO ()
testGetGhcVersionFail cradlePred rootDir file cradleFailPred step =
  testCradle cradlePred rootDir file step $ \crd _ -> do
    res <- getRuntimeGhcVersion crd

    case res of
      CradleSuccess _ -> liftIO $ assertFailure "Cradle loaded successfully"
      CradleNone -> liftIO $ assertFailure "Unexpected none-Cradle"
      CradleFail crdFail -> liftIO $ cradleFailPred crdFail

testDirectoryFail :: (Cradle Void -> Bool) -> FilePath -> FilePath -> (CradleError -> Assertion) -> (String -> IO ()) -> IO ()
testDirectoryFail cradlePred rootDir file cradleFailPred step =
  testCradle cradlePred rootDir file step $ \crd fp ->
    testLoadFileCradleFail crd fp cradleFailPred step

testCradle :: (Cradle Void -> Bool) -> FilePath -> FilePath -> (String -> IO ()) -> (Cradle Void -> FilePath -> IO a) -> IO a
testCradle cradlePred rootDir file step cont = withTempCopy rootDir $ \rootDir' -> do
  fp <- canonicalizePath (rootDir' </> file)
  crd <- initialiseCradle cradlePred fp step
  step "Initialise Flags"
  cont crd fp

initialiseCradle :: (Cradle Void -> Bool) -> FilePath -> (String -> IO ()) -> IO (Cradle Void)
initialiseCradle cradlePred a_fp step = do
  step $ "Finding Cradle for: " ++ a_fp
  mcfg <- findCradle a_fp
  step $ "Loading Cradle: " ++ show mcfg
  crd <- case mcfg of
    Just cfg -> loadCradle cfg
    Nothing -> loadImplicitCradle a_fp
  cradlePred crd @? "Must be the correct kind of cradle"
  pure crd

testLoadFile :: Cradle a -> FilePath -> (String -> IO ()) -> IO ()
testLoadFile crd a_fp step = do
  libDirRes <- getRuntimeGhcLibDir crd
  handleCradleResult libDirRes $ \libDir ->
    withCurrentDirectory (cradleRootDir crd) $
      G.runGhc (Just libDir) $ do
        let relFp = makeRelative (cradleRootDir crd) a_fp
        liftIO (step "Cradle load")
        res <- initializeFlagsWithCradle mempty relFp crd
        handleCradleResult res $ \(ini, _) -> do
          liftIO (step "Initial module load")
          sf <- ini
          case sf of
            -- Test resetting the targets
            Succeeded -> do
              liftIO (step "Set target files")
              setTargetFiles mempty [(a_fp, a_fp)]
            Failed -> liftIO $ assertFailure "Module loading failed"

testLoadFileCradleFail :: Cradle a -> FilePath -> (CradleError -> Assertion) -> (String -> IO ()) -> IO ()
testLoadFileCradleFail crd a_fp cradleErrorExpectation step = do
  step "Loading cradle"
  -- don't spin up a ghc session, just run the opts program manually since
  -- we're not guaranteed to be able to get the ghc libdir if the cradle is
  -- failing
  withCurrentDirectory (cradleRootDir crd) $ do
    let relFp = makeRelative (cradleRootDir crd) a_fp
    res <- runCradle (cradleOptsProg crd) mempty relFp
    case res of
      CradleSuccess _ -> liftIO $ assertFailure "Cradle loaded successfully"
      CradleNone -> liftIO $ assertFailure "Unexpected none-Cradle"
      CradleFail crdFail -> liftIO $ cradleErrorExpectation crdFail

testLoadCradleDependencies :: (Cradle Void -> Bool) -> FilePath -> FilePath -> ([FilePath] -> Assertion) -> (String -> IO ()) -> IO ()
testLoadCradleDependencies cradlePred rootDir file dependencyPred step =
  withTempCopy rootDir $ \rootDir' -> do
    a_fp <- canonicalizePath (rootDir' </> file)
    crd <- initialiseCradle cradlePred a_fp step
    libDirRes <- getRuntimeGhcLibDir crd
    handleCradleResult libDirRes $ \libDir -> do
      step "Initialise Flags"
      withCurrentDirectory (cradleRootDir crd) $
        G.runGhc (Just libDir) $ do
          let relFp = makeRelative (cradleRootDir crd) a_fp
          res <- initializeFlagsWithCradleWithMessage mempty (Just (\_ n _ _ -> step (show n))) relFp crd
          handleCradleResult res $ \(_, options) ->
            liftIO $ dependencyPred (componentDependencies options)

handleCradleResult :: MonadIO m => CradleLoadResult a -> (a -> m ()) -> m ()
handleCradleResult (CradleSuccess x) f = f x
handleCradleResult CradleNone _ = liftIO $ assertFailure "Unexpected none-Cradle"
handleCradleResult (CradleFail (CradleError _deps _ex stde)) _ =
  liftIO $ assertFailure ("Unexpected cradle fail" ++ unlines stde)

findCradleForModule :: FilePath -> Maybe FilePath -> (String -> IO ()) -> IO ()
findCradleForModule fp expected' step = do
  expected <- maybe (return Nothing) (fmap Just . canonicalizePath) expected'
  a_fp <- canonicalizePath fp
  step "Finding cradle"
  crd <- findCradle a_fp
  crd @?= expected

testImplicitCradle :: FilePath -> FilePath -> ActionName Void -> (String -> IO ()) -> IO ()
testImplicitCradle rootDir file expectedActionName step =
  withTempCopy rootDir $ \rootDir' -> do
    fp <- makeAbsolute (rootDir' </> file)
    step "Inferring implicit cradle"
    crd <- loadImplicitCradle fp :: IO (Cradle Void)

    actionName (cradleOptsProg crd) @?= expectedActionName

    expectedCradleRootDir <- makeAbsolute rootDir'
    cradleRootDir crd @?= expectedCradleRootDir

    step "Initialize flags"
    testLoadFile crd fp step
