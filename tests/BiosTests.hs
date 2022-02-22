{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure
import qualified Test.Tasty.Options as Tasty
import qualified Test.Tasty.Ingredients as Tasty
import qualified GHC as G
import HIE.Bios
import HIE.Bios.Ghc.Api
import HIE.Bios.Ghc.Load
import HIE.Bios.Cradle
import HIE.Bios.Environment
import HIE.Bios.Types
import Control.Monad.IO.Class
import Control.Monad ( forM_, unless )
import Data.List ( sort, isPrefixOf, isInfixOf )
import Data.Typeable
import Data.Void
import System.Directory
import System.FilePath (addTrailingPathSeparator,  makeRelative, (</>) )
import System.Info.Extra ( isWindows )
import System.IO.Temp
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import Control.Monad.Extra (unlessM)
import qualified HIE.Bios.Ghc.Gap as Gap

argDynamic :: [String]
argDynamic = ["-dynamic" | Gap.hostIsDynamic]

-- | This ghc version is assumed to be tested by CI to validate
-- the "with-compiler" field is honoured by hie-bios.
--
-- If you change this version, make sure to also update 'cabal.project'
-- in 'tests\/projects\/cabal-with-ghc'.
extraGhcVersion :: String
extraGhcVersion = "8.10.7"

extraGhc :: String
extraGhc = "ghc-" ++ extraGhcVersion

main :: IO ()
main = do
  writeStackYamlFiles
  stackDep <- checkToolIsAvailable "stack"
  cabalDep <- checkToolIsAvailable "cabal"
  extraGhcDep <- checkToolIsAvailable extraGhc

  defaultMainWithIngredients (ignoreToolTests:defaultIngredients) $
    testGroup "Bios-tests"
      [ testGroup "Find cradle"
        [ testCaseSteps "simple-cabal"
                (findCradleForModule
                  "./tests/projects/simple-cabal/B.hs"
                  (Just "./tests/projects/simple-cabal/hie.yaml")
                )

        -- Checks if we can find a hie.yaml even when the given filepath
        -- is unknown. This functionality is required by Haskell IDE Engine.
        , testCaseSteps "simple-cabal-unknown-path"
                (findCradleForModule
                  "./tests/projects/simple-cabal/Foo.hs"
                  (Just "./tests/projects/simple-cabal/hie.yaml")
                )
        ]
      , testGroup "Symlink"
        [ testCaseSteps "Can load base module" $ \step -> do
            withTempCopy "./tests/projects/symlink-test" $ \tmpdir -> do
              crdl <- initialiseCradle isMultiCradle (addTrailingPathSeparator tmpdir) step
              step "Load module A"
              withCurrentDirectory (cradleRootDir crdl) $ do
                runCradle (cradleOptsProg crdl) (const (pure ())) "./a/A.hs"
                >>= \case
                  CradleSuccess r ->
                    componentOptions r `shouldMatchList` ["a"] <> argDynamic
                  _ -> assertFailure "Cradle could not be loaded"

        , testCaseSteps "Can load symlinked module" $ \step -> do
            withTempCopy "./tests/projects/symlink-test" $ \tmpdir -> do
              crdl <- initialiseCradle isMultiCradle (addTrailingPathSeparator tmpdir) step
              step "Attemp to load symlinked module A"
              withCurrentDirectory (cradleRootDir crdl) $ do
                createDirectoryLink "./a" "./b"
                unlessM (doesFileExist "./b/A.hs")
                  $ assertFailure "Test invariant broken, this file must exist."

                runCradle (cradleOptsProg crdl) (const (pure ())) "./b/A.hs"
                >>= \case
                  CradleSuccess r ->
                    componentOptions r `shouldMatchList` ["b"] <> argDynamic
                  _ -> assertFailure "Cradle could not be loaded"

        , testCaseSteps "Can not load symlinked module that is ignored" $ \step -> do
            withTempCopy "./tests/projects/symlink-test" $ \tmpdir -> do
              crdl <- initialiseCradle isMultiCradle (addTrailingPathSeparator tmpdir) step
              step "Attemp to load symlinked module A"
              withCurrentDirectory (cradleRootDir crdl) $ do
                createDirectoryLink "./a" "./c"
                unlessM (doesFileExist "./c/A.hs")
                  $ assertFailure "Test invariant broken, this file must exist."

                runCradle (cradleOptsProg crdl) (const (pure ())) "./c/A.hs"
                  >>= \case
                    CradleNone -> pure ()
                    _ -> assertFailure "Cradle loaded symlink"
        ]
      , testGroup "Loading tests"
        [ testGroup "bios" biosTestCases
        , testGroup "direct" directTestCases
        , testGroupWithDependency cabalDep (cabalTestCases extraGhcDep)
        , ignoreOnGhc9AndNewer $ testGroupWithDependency stackDep stackTestCases
        ]
      ]

linuxExlusiveTestCases :: [TestTree]
linuxExlusiveTestCases
  | not isWindows
  = [ testCaseSteps "simple-bios" $ testDirectory isBiosCradle "./tests/projects/simple-bios" "B.hs"
    , testCaseSteps "simple-bios-ghc" $ testDirectory isBiosCradle "./tests/projects/simple-bios-ghc" "B.hs"
    , testCaseSteps "simple-bios-deps" $ testLoadCradleDependencies isBiosCradle "./tests/projects/simple-bios" "B.hs" (assertEqual "dependencies" ["hie-bios.sh", "hie.yaml"])
    , testCaseSteps "simple-bios-deps-new" $ testLoadCradleDependencies isBiosCradle "./tests/projects/deps-bios-new" "B.hs" (assertEqual "dependencies" ["hie-bios.sh", "hie.yaml"])
    ]
  | otherwise
  = []

cabalTestCases :: ToolDependency -> [TestTree]
cabalTestCases extraGhcDep =
  [ testCaseSteps "failing-cabal" $ testDirectoryFail isCabalCradle "./tests/projects/failing-cabal" "MyLib.hs"
    (\CradleError {..} -> do
        cradleErrorExitCode @?= ExitFailure 1
        cradleErrorDependencies `shouldMatchList` ["failing-cabal.cabal", "cabal.project", "cabal.project.local"])
  , testCaseSteps "simple-cabal" $ testDirectory isCabalCradle "./tests/projects/simple-cabal" "B.hs"
  , testCaseSteps "nested-cabal" $ testLoadCradleDependencies isCabalCradle "./tests/projects/nested-cabal" "sub-comp/Lib.hs"
    (\deps -> deps `shouldMatchList` ["sub-comp" </> "sub-comp.cabal", "cabal.project", "cabal.project.local"]
    )
  , testCaseSteps "nested-cabal2" $ testLoadCradleDependencies isCabalCradle "./tests/projects/nested-cabal" "MyLib.hs"
    (\deps -> deps `shouldMatchList` ["nested-cabal.cabal", "cabal.project", "cabal.project.local"]
    )
  , testCaseSteps "multi-cabal" {- tests if both components can be loaded -}
                $  testDirectory isCabalCradle "./tests/projects/multi-cabal" "app/Main.hs"
                >> testDirectory isCabalCradle "./tests/projects/multi-cabal" "src/Lib.hs"
  , testCaseSteps "monorepo-cabal" {- issue https://github.com/mpickering/hie-bios/issues/200 -}
                $  testDirectory isCabalCradle "./tests/projects/monorepo-cabal" "A/Main.hs"
                >> testDirectory isCabalCradle "./tests/projects/monorepo-cabal" "B/MyLib.hs"
  , testGroup "Implicit cradle tests" $
        [ testCaseSteps "implicit-cabal" $
            testImplicitCradle "./tests/projects/implicit-cabal" "Main.hs" Cabal
        , testCaseSteps "implicit-cabal-no-project" $
            testImplicitCradle "./tests/projects/implicit-cabal-no-project" "Main.hs" Cabal
        , testCaseSteps "implicit-cabal-deep-project" $
            testImplicitCradle "./tests/projects/implicit-cabal-deep-project" "foo/Main.hs" Cabal
        ]
  , testGroupWithDependency extraGhcDep
    [ expectFailBecause "hie-bios does not honour ghc in cabal.project"
      $ testCaseSteps "Appropriate ghc and libdir" $ \step -> do
        fp <- canonicalizePath "./tests/projects/cabal-with-ghc/src/MyLib.hs"
        crd <- initialiseCradle isCabalCradle fp step
        step "Get runtime GHC library directory"
        testGetGhcLibDir crd extraGhcVersion
        step "Get runtime GHC version"
        testGetGhcVersion crd extraGhcVersion
    ]
  ]

stackTestCases :: [TestTree]
stackTestCases =
  [ expectFailBecause "stack repl does not fail on an invalid cabal file" $
      testCaseSteps "failing-stack" $ testDirectoryFail isStackCradle "./tests/projects/failing-stack" "src/Lib.hs"
        (\CradleError {..} -> do
            cradleErrorExitCode @?= ExitFailure 1
            cradleErrorDependencies `shouldMatchList` ["failing-stack.cabal", "stack.yaml", "package.yaml"])
  , testCaseSteps "simple-stack" $ testDirectory isStackCradle "./tests/projects/simple-stack" "B.hs"
  , testCaseSteps "multi-stack" {- tests if both components can be loaded -}
                $  testDirectory isStackCradle "./tests/projects/multi-stack" "app/Main.hs"
                >> testDirectory isStackCradle "./tests/projects/multi-stack" "src/Lib.hs"
  , testCaseSteps "nested-stack" $ testLoadCradleDependencies isStackCradle "./tests/projects/nested-stack" "sub-comp/Lib.hs"
        (\deps -> deps `shouldMatchList`
          ["sub-comp" </> "sub-comp.cabal", "sub-comp" </> "package.yaml", "stack.yaml"]
        )
  , testCaseSteps "nested-stack2" $ testLoadCradleDependencies isStackCradle "./tests/projects/nested-stack" "MyLib.hs"
      (\deps -> deps `shouldMatchList` ["nested-stack.cabal", "package.yaml", "stack.yaml"]
      )
  , testCaseSteps "stack-with-yaml" {- tests if both components can be loaded -}
                $  testDirectory isStackCradle "./tests/projects/stack-with-yaml" "app/Main.hs"
                >> testDirectory isStackCradle "./tests/projects/stack-with-yaml" "src/Lib.hs"
  , testCaseSteps "multi-stack-with-yaml" {- tests if both components can be loaded -}
                $  testDirectory isStackCradle "./tests/projects/multi-stack-with-yaml" "appA/src/Lib.hs"
                >> testDirectory isStackCradle "./tests/projects/multi-stack-with-yaml" "appB/src/Lib.hs"
  ,
    -- Test for special characters in the path for parsing of the ghci-scripts.
    -- Issue https://github.com/mpickering/hie-bios/issues/162
    testCaseSteps "space stack"
                  $  testDirectory isStackCradle "./tests/projects/space stack" "A.hs"
                  >> testDirectory isStackCradle "./tests/projects/space stack" "B.hs"
  , testGroup "Implicit cradle tests"
      [ testCaseSteps "implicit-stack" $
          testImplicitCradle "./tests/projects/implicit-stack" "Main.hs" Stack
      , testCaseSteps "implicit-stack-multi"
          $ testImplicitCradle "./tests/projects/implicit-stack-multi" "Main.hs" Stack
          >> testImplicitCradle "./tests/projects/implicit-stack-multi" "other-package/Main.hs" Stack
      ]
  ]

biosTestCases :: [TestTree]
biosTestCases =
  [ testCaseSteps "failing-bios" $ testDirectoryFail isBiosCradle "./tests/projects/failing-bios" "B.hs"
    (\CradleError {..} -> do
        cradleErrorExitCode @?= ExitFailure 1
        cradleErrorDependencies `shouldMatchList` ["hie.yaml"])
  , testCaseSteps "failing-bios-ghc" $ testGetGhcVersionFail isBiosCradle "./tests/projects/failing-bios-ghc" "B.hs"
    (\CradleError {..} -> do
        cradleErrorExitCode @?= ExitSuccess
        cradleErrorDependencies `shouldMatchList` []
        length cradleErrorStderr @?= 1
        "Couldn't execute myGhc" `isPrefixOf` head cradleErrorStderr @? "Error message should contain basic information" )
  , testCaseSteps "simple-bios-shell" $ testDirectory isBiosCradle "./tests/projects/simple-bios-shell" "B.hs"
  , testCaseSteps "simple-bios-shell-deps" $ testLoadCradleDependencies isBiosCradle "./tests/projects/simple-bios-shell" "B.hs" (assertEqual "dependencies" ["hie.yaml"])
  ]

directTestCases :: [TestTree]
directTestCases =
  [ testCaseSteps "simple-direct" $ testDirectory isDirectCradle "./tests/projects/simple-direct" "B.hs"
  , testCaseSteps "multi-direct" {- tests if both components can be loaded -}
      $  testDirectory isMultiCradle "./tests/projects/multi-direct" "A.hs"
      >> testDirectory isMultiCradle "./tests/projects/multi-direct" "B.hs"
  ]

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

-- | Here we are testing that the cradle's method of obtaining the ghcLibDir
-- always works.
testGetGhcLibDir :: Cradle a -> String -> IO ()
testGetGhcLibDir crd ghcVersion = do
  libDirRes <- getRuntimeGhcLibDir crd
  isSuccess libDirRes
  where
    -- heuristically test that the produced $libdir makes sense.
    isSuccess (CradleSuccess path) =
      ghcVersion `isInfixOf` path @? "Expected \"" ++ ghcVersion
                                     ++ "\" to be infix of: " ++ path
    isSuccess _ =
      assertFailure "Must succeed loading ghc lib directory"

-- | Here we are testing that the cradle's method of getting the runtime ghc
-- version is correct - which while testing, should be the version that we have
-- built the tests with. This will fail if you compiled the tests with a ghc
-- that doesn't equal the ghc on your path though :(
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
      CradleFail crdlFail -> liftIO $ cradleFailPred crdlFail

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
        res <- initializeFlagsWithCradleWithMessage (Just (\_ n _ _ -> step (show n))) relFp crd
        handleCradleResult res $ \(ini, _) -> do
          liftIO (step "Initial module load")
          sf <- ini
          case sf of
            -- Test resetting the targets
            Succeeded -> setTargetFilesWithMessage (Just (\_ n _ _ -> step (show n))) [(a_fp, a_fp)]
            Failed -> liftIO $ assertFailure "Module loading failed"

testLoadFileCradleFail :: Cradle a -> FilePath -> (CradleError -> Assertion) -> (String -> IO ()) -> IO ()
testLoadFileCradleFail crd a_fp cradleErrorExpectation step = do
  -- don't spin up a ghc session, just run the opts program manually since
  -- we're not guaranteed to be able to get the ghc libdir if the cradle is
  -- failing
  withCurrentDirectory (cradleRootDir crd) $ do
    let relFp = makeRelative (cradleRootDir crd) a_fp
    res <- runCradle (cradleOptsProg crd) (step . show) relFp
    case res of
      CradleSuccess _ -> liftIO $ assertFailure "Cradle loaded successfully"
      CradleNone -> liftIO $ assertFailure "Unexpected none-Cradle"
      CradleFail crdlFail -> liftIO $ cradleErrorExpectation crdlFail

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
          res <- initializeFlagsWithCradleWithMessage (Just (\_ n _ _ -> step (show n))) relFp crd
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
  crdl <- findCradle a_fp
  crdl @?= expected

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
  where ignored = ["dist", "dist-newstyle", ".stack-work"]

-- ------------------------------------------------------------------
-- Helper functions
-- ------------------------------------------------------------------

shouldMatchList :: (Show a, Ord a) => [a] -> [a] -> Assertion
shouldMatchList xs ys = sort xs @?= sort ys

infix 1 `shouldMatchList`

writeStackYamlFiles :: IO ()
writeStackYamlFiles =
  forM_ stackProjects $ \(proj, syaml, pkgs) ->
    writeFile (proj </> syaml) (stackYaml stackYamlResolver pkgs)

stackProjects :: [(FilePath, FilePath, [FilePath])]
stackProjects =
  [ ("tests" </> "projects" </> "multi-stack", "stack.yaml", ["."])
  , ("tests" </> "projects" </> "failing-stack", "stack.yaml", ["."])
  , ("tests" </> "projects" </> "simple-stack", "stack.yaml", ["."])
  , ("tests" </> "projects" </> "nested-stack", "stack.yaml", [".", "./sub-comp"])
  , ("tests" </> "projects" </> "space stack", "stack.yaml", ["."])
  , ("tests" </> "projects" </> "implicit-stack", "stack.yaml", ["."])
  , ("tests" </> "projects" </> "implicit-stack-multi", "stack.yaml", ["."])
  , ("tests" </> "projects" </> "implicit-stack-multi", "stack.yaml", ["."])
  , ("tests" </> "projects" </> "multi-stack-with-yaml", "stack-alt.yaml", ["appA", "appB"])
  , ("tests" </> "projects" </> "stack-with-yaml", "stack-alt.yaml", ["."])
  ]

stackYaml :: String -> [FilePath] -> String
stackYaml resolver pkgs = unlines
  $ ["resolver: " ++ resolver, "packages:"]
  ++ map ("- " ++) pkgs

stackYamlResolver :: String
stackYamlResolver =
#if (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(9,0,1,0)))
  "nightly-2021-08-22" -- GHC 9.0.1
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,10,1,0)))
  "lts-18.6" -- GHC 8.10.4
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,8,1,0)))
  "lts-16.31" -- GHC 8.8.4
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,6,5,0)))
  "lts-14.27" -- GHC 8.6.5
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,6,4,0)))
  "lts-13.19" -- GHC 8.6.4
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,4,4,0)))
  "lts-12.26" -- GHC 8.4.4
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,4,3,0)))
  "lts-12.14" -- GHC 8.4.3
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,2,2,0)))
  "lts-11.22" -- GHC 8.2.2
#endif

-- ------------------------------------------------------------------
-- Most tests have some run-time tool dependencies.
-- We only want to run tests if these tools are available.
-- ------------------------------------------------------------------

data ToolDependency = ToolDependency
  { toolName :: String
  , toolExists :: Bool
  }

checkToolIsAvailable :: String -> IO ToolDependency
checkToolIsAvailable f = do
  exists <- maybe False (const True) <$> findExecutable f
  pure ToolDependency
    { toolName = f
    , toolExists = exists
    }

testGroupWithDependency :: ToolDependency -> [TestTree] -> TestTree
testGroupWithDependency td tc = askOption @IgnoreToolDeps (\case
  IgnoreToolDeps ignoreToolDep
    | ignoreToolDep || toolExists td -> tg
    | otherwise -> itg
  )
  where
    tg = testGroup (toolName td) tc

    itg =
      ignoreTestBecause
        ("These tests require that the following" ++
        " tool can be found on the path: " ++ toolName td)
        tg

-- ------------------------------------------------------------------
-- Run test-suite ignoring run-time tool dependencies.
-- Can be used to force CI to run the whole test-suite.
-- Makes sure that the full test-suite is being run on a properly configured
-- environment.
-- ------------------------------------------------------------------

-- | This option, when set to 'True', specifies that we should run in the
-- «list tests» mode
newtype IgnoreToolDeps = IgnoreToolDeps Bool
  deriving (Eq, Ord, Typeable)

instance Tasty.IsOption IgnoreToolDeps where
  defaultValue = IgnoreToolDeps False
  parseValue = fmap IgnoreToolDeps . Tasty.safeReadBool
  optionName = pure "ignore-tool-deps"
  optionHelp = pure "Run tests whether their tool dependencies exist or not"
  optionCLParser = Tasty.flagCLParser Nothing (IgnoreToolDeps True)

-- | The ingredient that provides the test listing functionality
ignoreToolTests :: Tasty.Ingredient
ignoreToolTests = Tasty.TestManager [Tasty.Option (Proxy :: Proxy IgnoreToolDeps)] $
  \opts _tree ->
    case Tasty.lookupOption opts of
      IgnoreToolDeps False -> Nothing
      IgnoreToolDeps True -> Just $ pure True

-- ------------------------------------------------------------------
-- Ignore test group if built with GHC 9 or newer
-- ------------------------------------------------------------------

ignoreOnGhc9AndNewer :: TestTree -> TestTree
ignoreOnGhc9AndNewer tt =
#if (defined(MIN_VERSION_GLASGOW_HASKELL) && MIN_VERSION_GLASGOW_HASKELL(9,0,0,0))
  ignoreTestBecause "Not supported on GHC >= 9"
#endif
  tt

