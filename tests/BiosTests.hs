{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Utils

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure
import qualified Test.Tasty.Options as Tasty
import qualified Test.Tasty.Ingredients as Tasty
import HIE.Bios
import HIE.Bios.Cradle
import HIE.Bios.Cradle.ProgramVersions (ProgramVersions(..), makeVersions, runCachedIO)
import HIE.Bios.Types (runGhcCmd)
import Control.Monad (forM_)
import Control.Monad.Extra (unlessM)
import Control.Monad.IO.Class
import Data.Foldable (for_)
import Data.List ( sort, isPrefixOf )
import Data.Maybe ( isJust )
import Data.Typeable
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Directory
import System.FilePath ((</>), makeRelative)
import System.Info.Extra (isWindows)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)
import qualified HIE.Bios.Ghc.Gap as Gap


argDynamic :: [String]
argDynamic = ["-dynamic" | Gap.hostIsDynamic]

-- | This ghc version is assumed to be tested by CI to validate
-- the "with-compiler" field is honoured by hie-bios.
--
-- If you change this version, make sure to also update 'cabal.project'
-- in 'tests\/projects\/cabal-with-ghc'.
extraGhcVersion :: String
extraGhcVersion = "9.4.8"

extraGhc :: String
extraGhc = "ghc-" ++ extraGhcVersion

-- | To get all logs, run the tests via:
--
-- @
-- cabal run test:bios-tests --test-options="--debug"
-- @
--
-- or
--
-- @
-- TASTY_DEBUG=TRUE cabal test test:bios-tests
-- @
--
-- to avoid recompilation.
main :: IO ()
main = do
  for_ [stderr, stdout] (`hSetBuffering` LineBuffering)
  writeStackYamlFiles
  stackDep <- checkToolIsAvailable "stack"
  cabalDep <- checkToolIsAvailable "cabal"
  extraGhcDep <- checkToolIsAvailable extraGhc

  defaultMainWithIngredients (ignoreToolTests:verboseLogging:defaultIngredients) $
    testGroup "Bios-tests"
      [ testGroup "Find cradle" findCradleTests
      , testGroup "Symlink" symbolicLinkTests
      , testGroup "Loading tests"
        [ testGroup "bios" biosTestCases
        , testGroup "direct" directTestCases
        , testGroupWithDependency cabalDep (cabalTestCases extraGhcDep)
        , ignoreOnUnsupportedGhc $ testGroupWithDependency stackDep stackTestCases
        ]
      ]

symbolicLinkTests :: [TestTree]
symbolicLinkTests =
  [ biosTestCase "Can load base module" $ runTestEnv "./symlink-test" $ do
      initCradle "doesNotExist.hs"
      assertCradle isMultiCradle
      step "Attempt to load symlinked module A"
      do
        loadComponentOptions "./a/A.hs"
        assertComponentOptions $ \opts ->
          componentOptions opts `shouldMatchList` ["a"] <> argDynamic

  , biosTestCase "Can load symlinked module" $ runTestEnv "./symlink-test" $ do
      initCradle "doesNotExist.hs"
      assertCradle isMultiCradle
      step "Attempt to load symlinked module A"
      do
        cradle <- askCradle
        let rooted = (cradleRootDir cradle </>)
        liftIO $ createDirectoryLink (rooted "a") (rooted "./b")
        liftIO $ unlessM (doesFileExist $ rooted "b/A.hs") $
          assertFailure "Test invariant broken, this file must exist."
        loadComponentOptions "./b/A.hs"
        assertComponentOptions $ \opts ->
          componentOptions opts `shouldMatchList` ["b"] <> argDynamic

  , biosTestCase "Can not load symlinked module that is ignored" $ runTestEnv "./symlink-test" $ do
      initCradle "doesNotExist.hs"
      assertCradle isMultiCradle
      step "Attempt to load symlinked module A"
      do
        cradle <- askCradle
        let rooted = (cradleRootDir cradle </>)
        liftIO $ createDirectoryLink (rooted "./a") (rooted "./c")
        liftIO $ unlessM (doesFileExist $ rooted "c/A.hs") $
          assertFailure "Test invariant broken, this file must exist."
        loadComponentOptions "./c/A.hs"
        assertLoadNone
  ]

biosTestCases :: [TestTree]
biosTestCases =
  [ biosTestCase "failing-bios" $ runTestEnv "./failing-bios" $ do
      initCradle "B.hs"
      assertCradle isBiosCradle
      loadComponentOptions "B.hs"
      assertCradleError $ \CradleError {..} -> do
        cradleErrorExitCode @?= ExitFailure 1
        cradleErrorDependencies `shouldMatchList` ["hie.yaml"]
  , biosTestCase "failing-bios-ghc" $ runTestEnv "./failing-bios-ghc" $ do
      initCradle "B.hs"
      assertCradle isBiosCradle
      loadRuntimeGhcVersion
      ghcVersionLR <- askGhcVersionResult
      assertCradleLoadError ghcVersionLR >>= \CradleError {..} -> liftIO $ do
        cradleErrorExitCode @?= ExitSuccess
        cradleErrorDependencies `shouldMatchList` []
        length cradleErrorStderr @?= 1
        forM_ cradleErrorStderr $ \errorCtx ->
          -- On windows, this error message contains '"' around the executable name
          if isWindows
            then "Couldn't execute \"myGhc\"" `isPrefixOf` errorCtx @? "Error message should contain error information"
            else "Couldn't execute myGhc"     `isPrefixOf` errorCtx @? "Error message should contain error information"
  , biosTestCase "simple-bios-shell" $ runTestEnv "./simple-bios-shell" $ do
      testDirectoryM isBiosCradle "B.hs"
  , biosTestCase "simple-bios-shell-deps" $ runTestEnv "./simple-bios-shell" $ do
      biosCradleDeps "B.hs" ["hie.yaml"]
  ] <> concat
    -- Regression test for https://github.com/haskell/hie-bios/issues/498
    -- Tests that GHC version detection succeeds when the GHC wrapper produces
    -- extra output (e.g. darcs "WARNING: creating a nested repository.").
    [ [ biosTestCase "noisy-bios-ghc" $ runTestEnv "./noisy-bios-ghc" $ do
          initCradle "B.hs"
          assertCradle isBiosCradle
          -- Test that getRuntimeGhcVersion handles noisy output
          loadRuntimeGhcVersion
          assertGhcVersion
          -- Test that makeVersions/getGhcVersion (used during cradle discovery)
          -- also handles noisy output via versionMaybe
          crd <- askCradle
          root <- askRoot
          logger <- askLogger
          versions <- liftIO $ makeVersions logger root ((runGhcCmd . cradleOptsProg) crd)
          ghcVer <- liftIO $ runCachedIO (ghcVersion versions)
          liftIO $ isJust ghcVer @? "GHC version detection should succeed with noisy output"
      ] | not isWindows
    ] <> concat [linuxTestCases | False] -- TODO(fendor), enable again
  where
    biosCradleDeps :: FilePath -> [FilePath] -> TestM ()
    biosCradleDeps fp deps = do
      initCradle fp
      assertCradle isBiosCradle
      loadComponentOptions fp
      assertComponentOptions $ \opts -> do
        deps @?= componentDependencies opts

    linuxTestCases =
      [ biosTestCase "simple-bios" $ runTestEnv "./simple-bios" $
          testDirectoryM isBiosCradle "B.hs"
      , biosTestCase "simple-bios-ghc" $ runTestEnv "./simple-bios-ghc" $
          testDirectoryM isBiosCradle  "B.hs"
      , biosTestCase "simple-bios-deps" $ runTestEnv "./simple-bios" $ do
          biosCradleDeps "B.hs" ["hie-bios.sh", "hie.yaml"]
      , biosTestCase "simple-bios-deps-new" $ runTestEnv "./deps-bios-new" $ do
          biosCradleDeps "B.hs" ["hie-bios.sh", "hie.yaml"]
      ]

cabalTestCases :: ToolDependency -> [TestTree]
cabalTestCases extraGhcDep =
  [
    biosTestCase "failing-cabal" $ runTestEnv "./failing-cabal" $ do
      cabalAttemptLoad "MyLib.hs"
      assertCradleError (\CradleError {..} -> do
        cradleErrorExitCode @?= ExitFailure 1
        cradleErrorDependencies `shouldMatchList` ["failing-cabal.cabal", "cabal.project", "cabal.project.local"])
  , biosTestCase "failing-cabal-multi-repl-with-shrink-error-files" $ runTestEnv "./failing-multi-repl-cabal-project" $ do
      cabalAttemptLoadFiles "multi-repl-cabal-fail/app/Main.hs" ["multi-repl-cabal-fail/src/Lib.hs", "multi-repl-cabal-fail/src/Fail.hs", "NotInPath.hs"]
      root <- askRoot
      multiSupported <- isCabalMultipleCompSupported'
      if multiSupported
        then
          assertCradleError (\CradleError {..} -> do
            cradleErrorExitCode @?= ExitFailure 1
            cradleErrorDependencies `shouldMatchList` ["cabal.project","cabal.project.local","multi-repl-cabal-fail.cabal"]
            -- NotInPath.hs does not match the cradle for `app/Main.hs`, so it should not be tried.
            (makeRelative root <$> cradleErrorLoadingFiles) `shouldMatchList` ["multi-repl-cabal-fail/app/Main.hs","multi-repl-cabal-fail/src/Fail.hs","multi-repl-cabal-fail/src/Lib.hs"])
        else assertLoadSuccess >>= \ComponentOptions {} -> do
          return ()
  , biosTestCase "simple-cabal" $ runTestEnv "./simple-cabal" $ do
      testDirectoryM isCabalCradle "B.hs"
  , biosTestCase "nested-cabal" $ runTestEnv "./nested-cabal" $ do
      cabalAttemptLoad "sub-comp/Lib.hs"
      assertComponentOptions $ \opts -> do
        componentDependencies opts `shouldMatchList`
          [ "sub-comp" </> "sub-comp.cabal"
          , "cabal.project"
          , "cabal.project.local"
          ]
  , biosTestCase "nested-cabal2" $ runTestEnv "./nested-cabal" $ do
      cabalAttemptLoad "MyLib.hs"
      assertComponentOptions $ \opts -> do
        componentDependencies opts `shouldMatchList`
          [ "nested-cabal.cabal"
          , "cabal.project"
          , "cabal.project.local"
          ]
  , biosTestCase "nested-cabal multi-mode includes enclosing deps for extra files" $ runTestEnv "./nested-cabal" $ do
      -- Initialize cradle first, since capability checks use the current cradle.
      initCradle "sub-comp/Lib.hs"
      assertCradle isCabalCradle
      multiSupported <- isCabalMultipleCompSupported'
      if multiSupported
        then do
          loadComponentOptionsMultiStyle "sub-comp/Lib.hs" ["MyLib.hs"]
          assertComponentOptions $ \opts -> do
            -- Expect both the main component's cabal file and the enclosing cabal for the extra file,
            -- plus project files.
            componentDependencies opts `shouldMatchList`
              [ "sub-comp" </> "sub-comp.cabal"
              , "nested-cabal.cabal"
              , "cabal.project"
              , "cabal.project.local"
              ]
        else do
          -- On older cabal/ghc combos, multi-repl isn't supported; just ensure load succeeds.
          loadComponentOptions "sub-comp/Lib.hs"
          _ <- assertLoadSuccess
          pure ()
  , biosTestCase "nested-cabal multi-mode includes enclosing deps when extra file is subcomp" $ runTestEnv "./nested-cabal" $ do
      -- Initialize cradle at the top level, then treat the sub-component file as an extra file.
      initCradle "MyLib.hs"
      assertCradle isCabalCradle
      multiSupported <- isCabalMultipleCompSupported'
      if multiSupported
        then do
          loadComponentOptionsMultiStyle "MyLib.hs" ["sub-comp/Lib.hs"]
          assertComponentOptions $ \opts -> do
            componentDependencies opts `shouldMatchList`
              [ "nested-cabal.cabal"
              , "sub-comp" </> "sub-comp.cabal"
              , "cabal.project"
              , "cabal.project.local"
              ]
        else do
          loadComponentOptions "MyLib.hs"
          _ <- assertLoadSuccess
          pure ()
  , biosTestCase "multi-cabal" $ runTestEnv "./multi-cabal" $ do
      {- tests if both components can be loaded -}
      testDirectoryM isCabalCradle "app/Main.hs"
      testDirectoryM isCabalCradle "src/Lib.hs"
  , {- issue https://github.com/mpickering/hie-bios/issues/200 -}
    biosTestCase "monorepo-cabal" $ runTestEnv "./monorepo-cabal" $ do
      testDirectoryM isCabalCradle "A/Main.hs"
      testDirectoryM isCabalCradle "B/MyLib.hs"
  , testGroup "Implicit cradle tests" $
      [ biosTestCase "implicit-cabal" $ runTestEnv "./implicit-cabal" $ do
          testImplicitDirectoryM isCabalCradle "Main.hs"
      , biosTestCase "implicit-cabal-no-project" $ runTestEnv "./implicit-cabal-no-project" $ do
          testImplicitDirectoryM isCabalCradle "Main.hs"
      , biosTestCase "implicit-cabal-deep-project" $ runTestEnv "./implicit-cabal-deep-project" $ do
          testImplicitDirectoryM isCabalCradle "foo/Main.hs"
      ]
  , testGroupWithDependency extraGhcDep
    [ biosTestCase "Appropriate ghc and libdir" $ runTestEnvLocal "./cabal-with-ghc" $ do
        initCradle "src/MyLib.hs"
        assertCradle isCabalCradle
        loadRuntimeGhcLibDir
        assertLibDirVersionIs extraGhcVersion
        loadRuntimeGhcVersion
        assertGhcVersionIs extraGhcVersion
        step "Find Component Options"
        loadComponentOptions "src/MyLib.hs"
        _ <- assertLoadSuccess
        pure ()
    ]
  , testGroup "Cabal cabalProject"
    [ biosTestCase "cabal-with-project, options propagated" $ runTestEnv "cabal-with-project" $ do
        opts <- cabalLoadOptions "src/MyLib.hs"
        liftIO $ do
          "-O2" `elem` componentOptions opts
            @? "Options must contain '-O2'"
    , biosTestCase "cabal-with-project, load" $ runTestEnv "cabal-with-project" $ do
        testDirectoryM isCabalCradle "src/MyLib.hs"
    , biosTestCase "multi-cabal-with-project, options propagated" $ runTestEnv "multi-cabal-with-project" $ do
        optsAppA <- cabalLoadOptions "appA/src/Lib.hs"
        liftIO $ do
          "-O2" `elem` componentOptions optsAppA
            @? "Options must contain '-O2'"
        optsAppB <- cabalLoadOptions "appB/src/Lib.hs"
        liftIO $ do
          "-O2" `notElem` componentOptions optsAppB
            @? "Options must not contain '-O2'"
    , biosTestCase "multi-cabal-with-project, load" $ runTestEnv "multi-cabal-with-project" $ do
        testDirectoryM isCabalCradle "appB/src/Lib.hs"
        testDirectoryM isCabalCradle "appB/src/Lib.hs"
    , testGroupWithDependency extraGhcDep
      [ biosTestCase "Honours extra ghc setting" $ runTestEnv "cabal-with-ghc-and-project" $ do
          initCradle "src/MyLib.hs"
          assertCradle isCabalCradle
          loadRuntimeGhcLibDir
          assertLibDirVersionIs extraGhcVersion
          loadRuntimeGhcVersion
          assertGhcVersionIs extraGhcVersion
          step "Find Component Options"
          loadComponentOptions "src/MyLib.hs"
          _ <- assertLoadSuccess
          pure ()
      ]
    , biosTestCase "force older Cabal version in custom setup" $ runTestEnv "cabal-with-custom-setup" $ do
        -- Specifically tests whether cabal 3.16 works as expected with
        -- an older lib:Cabal version that doesn't support '--with-repl'.
        -- This test doesn't hurt for other cases as well, so we enable it for
        -- all configurations.
        testDirectoryM isCabalCradle "src/MyLib.hs"
    , biosTestCase "force older Cabal version in custom setup with multi mode" $ runTestEnv "cabal-with-custom-setup" $ do
        -- Specifically tests whether cabal 3.16 works as expected with
        -- an older lib:Cabal version that doesn't support '--with-repl'.
        -- This test doesn't hurt for other cases as well, so we enable it for
        -- all configurations.
        let target = "src/MyLib.hs"
        initCradle target
        assertCradle isCabalCradle
        loadRuntimeGhcLibDir
        assertLibDirVersion
        loadRuntimeGhcVersion
        assertGhcVersion
        -- suffices to force loading cabal's `--enable-multi-repl` codepath
        loadFileGhcMultiStyle target []
    ]
  ]
  where
    cabalAttemptLoad :: FilePath -> TestM ()
    cabalAttemptLoad fp = do
      initCradle fp
      assertCradle isCabalCradle
      loadComponentOptions fp

    cabalAttemptLoadFiles :: FilePath -> [FilePath] -> TestM ()
    cabalAttemptLoadFiles fp fps = do
      initCradle fp
      assertCradle isCabalCradle
      loadComponentOptionsMultiStyle fp fps

    cabalLoadOptions :: FilePath -> TestM ComponentOptions
    cabalLoadOptions fp = do
      initCradle fp
      assertCradle isCabalCradle
      loadComponentOptions fp
      assertLoadSuccess

stackTestCases :: [TestTree]
stackTestCases =
  [ expectFailBecause "stack repl does not fail on an invalid cabal file" $
      biosTestCase "failing-stack" $ runTestEnv "./failing-stack" $ do
        stackAttemptLoad "src/Lib.hs"
        assertCradleError $ \CradleError {..} -> do
            cradleErrorExitCode @?= ExitFailure 1
            cradleErrorDependencies `shouldMatchList` ["failing-stack.cabal", "stack.yaml", "package.yaml"]
  , biosTestCase "simple-stack" $ runTestEnv "./simple-stack" $ do
      testDirectoryM isStackCradle "B.hs"
  , biosTestCase "multi-stack" $ runTestEnv "./multi-stack" $ do {- tests if both components can be loaded -}
      testDirectoryM isStackCradle "app/Main.hs"
      testDirectoryM isStackCradle "src/Lib.hs"
  , biosTestCase "nested-stack" $ runTestEnv "./nested-stack" $ do
      stackAttemptLoad "sub-comp/Lib.hs"
      assertComponentOptions $ \opts ->
        componentDependencies opts `shouldMatchList` ["sub-comp" </> "sub-comp.cabal", "sub-comp" </> "package.yaml", "stack.yaml"]
  , biosTestCase "nested-stack2" $ runTestEnv "./nested-stack" $ do
      stackAttemptLoad "MyLib.hs"
      assertComponentOptions $ \opts ->
        componentDependencies opts `shouldMatchList` ["nested-stack.cabal", "package.yaml", "stack.yaml"]
  , biosTestCase "stack-with-yaml" $ runTestEnv "./stack-with-yaml" $ do
      {- tests if both components can be loaded -}
      testDirectoryM isStackCradle "app/Main.hs"
      testDirectoryM isStackCradle "src/Lib.hs"
  , biosTestCase "multi-stack-with-yaml" $ runTestEnv "./multi-stack-with-yaml" $ do
      {- tests if both components can be loaded -}
      testDirectoryM isStackCradle "appA/src/Lib.hs"
      testDirectoryM isStackCradle "appB/src/Lib.hs"
  ,
    -- Test for special characters in the path for parsing of the ghci-scripts.
    -- Issue https://github.com/mpickering/hie-bios/issues/162
    biosTestCase "space stack" $ runTestEnv "./space stack" $ do
      testDirectoryM isStackCradle "A.hs"
      testDirectoryM isStackCradle "B.hs"
  , testGroup "Implicit cradle tests"
      [ biosTestCase "implicit-stack" $ runTestEnv "./implicit-stack" $
          testImplicitDirectoryM isStackCradle "Main.hs"
      , biosTestCase "implicit-stack-multi" $ runTestEnv "./implicit-stack-multi" $ do
          testImplicitDirectoryM isStackCradle "Main.hs"
          testImplicitDirectoryM isStackCradle "other-package/Main.hs"
      ]
  ]
  where
    stackAttemptLoad :: FilePath -> TestM ()
    stackAttemptLoad fp = do
      initCradle fp
      assertCradle isStackCradle
      loadComponentOptions fp

directTestCases :: [TestTree]
directTestCases =
  [ biosTestCase "simple-direct" $ runTestEnv  "./simple-direct" $ do
      testDirectoryM isDirectCradle "B.hs"
  , biosTestCase "multi-direct" $ runTestEnv "./multi-direct" $ do
      {- tests if both components can be loaded -}
      testDirectoryM isMultiCradle "A.hs"
      testDirectoryM isMultiCradle "B.hs"
  ]

findCradleTests :: [TestTree]
findCradleTests =
  [ cradleFileTest "Simple Existing File" "./simple-cabal"  "B.hs" (Just "hie.yaml")
  -- Checks if we can find a hie.yaml even when the given filepath
  -- is unknown. This functionality is required by Haskell IDE Engine.
  , cradleFileTest "Existing File" "cabal-with-ghc" "src/MyLib.hs" (Just "hie.yaml")
  , cradleFileTest "Non-existing file" "cabal-with-ghc" "src/MyLib2.hs" (Just "hie.yaml")
  , cradleFileTest "Non-existing file 2" "cabal-with-ghc" "MyLib2.hs" (Just "hie.yaml")
  , cradleFileTest "Directory 1" "cabal-with-ghc" "src/" (Just "hie.yaml")
  , cradleFileTest "Directory 2" "simple-cabal" "" (Just "hie.yaml")
  -- Unknown directory in a project, ought to work as well.
  , cradleFileTest "Directory 3" "simple-cabal" "src/" (Just "hie.yaml")
  , cradleFileTest "Directory does not exist" "doesnotexist" "A.hs" Nothing
  ]
  where
    cradleFileTest :: String -> FilePath -> FilePath -> Maybe FilePath -> TestTree
    cradleFileTest testName dir fpTarget result = biosTestCase testName $ do
      runTestEnv dir $ do
        findCradleForModuleM fpTarget result

-- ------------------------------------------------------------------
-- Unit-test Helper functions
-- ------------------------------------------------------------------

shouldMatchList :: (Show a, Ord a) => [a] -> [a] -> Assertion
shouldMatchList xs ys = sort xs @?= sort ys

infix 1 `shouldMatchList`

biosTestCase :: TestName -> (Bool -> Assertion) -> TestTree
biosTestCase name assertion = askOption @VerboseLogging (\case
  VerboseLogging verbose -> testCase name (assertion verbose)
  )

-- ------------------------------------------------------------------
-- Stack related helper functions
-- ------------------------------------------------------------------

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
#if (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(9,12,0,0)))
  "nightly-2025-08-07" -- GHC 9.12.2
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(9,10,0,0)))
  "lts-24.3" -- GHC 9.10.2
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(9,8,0,0)))
  "lts-23.19" -- GHC 9.8.4
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(9,6,0,0)))
  "lts-22.44" -- GHC 9.6.7
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(9,4,0,0)))
  "lts-21.25" -- GHC 9.4.8
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)))
  "lts-20.26" -- GHC 9.2.8
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
  deriving (Eq, Ord)

instance Tasty.IsOption IgnoreToolDeps where
  defaultValue = IgnoreToolDeps False
  parseValue = fmap IgnoreToolDeps . Tasty.safeReadBool
  optionName = pure "ignore-tool-deps"
  optionHelp = pure "Run tests whether their tool dependencies exist or not"
  optionCLParser = Tasty.flagCLParser Nothing (IgnoreToolDeps True)

-- | The ingredient that provides the "ignore missing run-time dependencies" functionality
ignoreToolTests :: Tasty.Ingredient
ignoreToolTests = Tasty.TestManager [Tasty.Option (Proxy :: Proxy IgnoreToolDeps)] $
  \_opts _tree -> Nothing

newtype VerboseLogging = VerboseLogging Bool

instance Tasty.IsOption VerboseLogging where
  defaultValue = VerboseLogging False
  parseValue = fmap VerboseLogging . Tasty.safeReadBool
  optionName = pure "debug"
  optionHelp = pure "Run the tests with verbose logging"
  optionCLParser = Tasty.flagCLParser Nothing (VerboseLogging True)

-- | The ingredient that provides the "ignore missing run-time dependencies" functionality
verboseLogging :: Tasty.Ingredient
verboseLogging = Tasty.TestManager [Tasty.Option (Proxy :: Proxy VerboseLogging)] $
  \_opts _tree -> Nothing

-- ------------------------------------------------------------------
-- Ignore test group if not supported by any stackage snapshot
-- ------------------------------------------------------------------

ignoreOnUnsupportedGhc :: TestTree -> TestTree
ignoreOnUnsupportedGhc tt =
#if (defined(MIN_VERSION_GLASGOW_HASKELL) && MIN_VERSION_GLASGOW_HASKELL(9,14,0,0))
  ignoreTestBecause "Not supported on GHC 9.14"
#endif
    tt
