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
import Control.Monad ( forM_ )
import Data.List ( sort, isPrefixOf )
import Data.Typeable
import System.Directory
import System.FilePath ((</>) )
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import Control.Monad.Extra (unlessM)
import qualified HIE.Bios.Ghc.Gap as Gap
import Control.Monad.IO.Class

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
        [ testCaseSteps "simple-cabal" $
            runTestEnvLocal "./simple-cabal" $ do
              findCradleForModuleM "B.hs" (Just "hie.yaml")

        -- Checks if we can find a hie.yaml even when the given filepath
        -- is unknown. This functionality is required by Haskell IDE Engine.
        , testCaseSteps "simple-cabal-unknown-path" $
            runTestEnvLocal "./simple-cabal" $ do
              findCradleForModuleM "Foo.hs" (Just "hie.yaml")
        ]
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
  [ testCaseSteps "Can load base module" $ runTestEnv "./symlink-test" $ do
      initCradle "doesNotExist.hs"
      assertCradle isMultiCradle
      step "Attempt to load symlinked module A"
      inCradleRootDir $ do
        loadComponentOptions "./a/A.hs"
        assertComponentOptions $ \opts ->
          componentOptions opts `shouldMatchList` ["a"] <> argDynamic

  , testCaseSteps "Can load symlinked module" $ runTestEnv "./symlink-test" $ do
      initCradle "doesNotExist.hs"
      assertCradle isMultiCradle
      step "Attempt to load symlinked module A"
      inCradleRootDir $ do
        liftIO $ createDirectoryLink "./a" "./b"
        liftIO $ unlessM (doesFileExist "./b/A.hs") $
          assertFailure "Test invariant broken, this file must exist."
        loadComponentOptions "./b/A.hs"
        assertComponentOptions $ \opts ->
          componentOptions opts `shouldMatchList` ["b"] <> argDynamic
  , testCaseSteps "Can not load symlinked module that is ignored" $ runTestEnv "./symlink-test" $ do
      initCradle "doesNotExist.hs"
      assertCradle isMultiCradle
      step "Attempt to load symlinked module A"
      inCradleRootDir $ do
        liftIO $ createDirectoryLink "./a" "./c"
        liftIO $ unlessM (doesFileExist "./c/A.hs") $
          assertFailure "Test invariant broken, this file must exist."
        loadComponentOptions "./c/A.hs"
        assertLoadNone
  ]

biosTestCases :: [TestTree]
biosTestCases =
  [ testCaseSteps "failing-bios" $ runTestEnv "./failing-bios" $ do
      initCradle "B.hs"
      assertCradle isBiosCradle
      loadComponentOptions "B.hs"
      assertCradleError $ \CradleError {..} -> do
        cradleErrorExitCode @?= ExitFailure 1
        cradleErrorDependencies `shouldMatchList` ["hie.yaml"]
  , testCaseSteps "failing-bios-ghc" $ runTestEnv "./failing-bios-ghc" $ do
      initCradle "B.hs"
      assertCradle isBiosCradle
      loadRuntimeGhcVersion
      ghcVersionLR <- askGhcVersionResult
      assertCradleLoadError ghcVersionLR >>= \CradleError {..} -> liftIO $ do
        cradleErrorExitCode @?= ExitSuccess
        cradleErrorDependencies `shouldMatchList` []
        length cradleErrorStderr @?= 1
        "Couldn't execute myGhc" `isPrefixOf` head cradleErrorStderr @? "Error message should contain basic information"
  , testCaseSteps "simple-bios-shell" $ runTestEnv "./simple-bios-shell" $ do
      testDirectoryM isBiosCradle "B.hs"
  , testCaseSteps "simple-bios-shell-deps" $ runTestEnv "./simple-bios-shell" $ do
      biosCradleDeps "B.hs" ["hie.yaml"]
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
      [ testCaseSteps "simple-bios" $ runTestEnv "./simple-bios" $
          testDirectoryM isBiosCradle "B.hs"
      , testCaseSteps "simple-bios-ghc" $ runTestEnv "./simple-bios-ghc" $
          testDirectoryM isBiosCradle  "B.hs"
      , testCaseSteps "simple-bios-deps" $ runTestEnv "./simple-bios" $ do
          biosCradleDeps "B.hs" ["hie-bios.sh", "hie.yaml"]
      , testCaseSteps "simple-bios-deps-new" $ runTestEnv "./deps-bios-new" $ do
          biosCradleDeps "B.hs" ["hie-bios.sh", "hie.yaml"]
      ]

cabalTestCases :: ToolDependency -> [TestTree]
cabalTestCases extraGhcDep =
  [ testCaseSteps "failing-cabal" $ runTestEnv "./failing-cabal" $ do
      cabalAttemptLoad "MyLib.hs"
      assertCradleError (\CradleError {..} -> do
        cradleErrorExitCode @?= ExitFailure 1
        cradleErrorDependencies `shouldMatchList` ["failing-cabal.cabal", "cabal.project", "cabal.project.local"])
  , testCaseSteps "simple-cabal" $ runTestEnv "./simple-cabal" $ do
      testDirectoryM isCabalCradle "B.hs"
  , testCaseSteps "nested-cabal" $ runTestEnv "./nested-cabal" $ do
      cabalAttemptLoad "sub-comp/Lib.hs"
      assertComponentOptions $ \opts -> do
        componentDependencies opts `shouldMatchList`
          [ "sub-comp" </> "sub-comp.cabal"
          , "cabal.project"
          , "cabal.project.local"
          ]
  , testCaseSteps "nested-cabal2" $ runTestEnv "./nested-cabal" $ do
      cabalAttemptLoad "MyLib.hs"
      assertComponentOptions $ \opts -> do
        componentDependencies opts `shouldMatchList`
          [ "nested-cabal.cabal"
          , "cabal.project"
          , "cabal.project.local"
          ]
  , testCaseSteps "multi-cabal" $ runTestEnv "./multi-cabal" $ do
      {- tests if both components can be loaded -}
      testDirectoryM isCabalCradle "app/Main.hs"
      testDirectoryM isCabalCradle "src/Lib.hs"
  , {- issue https://github.com/mpickering/hie-bios/issues/200 -}
    testCaseSteps "monorepo-cabal" $ runTestEnv "./monorepo-cabal" $ do
      testDirectoryM isCabalCradle "A/Main.hs"
      testDirectoryM isCabalCradle "B/MyLib.hs"
  , testGroup "Implicit cradle tests" $
      [ testCaseSteps "implicit-cabal" $ runTestEnv "./implicit-cabal" $ do
          testImplicitDirectoryM isCabalCradle "Main.hs"
      , testCaseSteps "implicit-cabal-no-project" $ runTestEnv "./implicit-cabal-no-project" $ do
          testImplicitDirectoryM isCabalCradle "Main.hs"
      , testCaseSteps "implicit-cabal-deep-project" $ runTestEnv "./implicit-cabal-deep-project" $ do
          testImplicitDirectoryM isCabalCradle "foo/Main.hs"
      ]
  , testGroupWithDependency extraGhcDep
    [ testCaseSteps "Appropriate ghc and libdir" $ runTestEnvLocal "./cabal-with-ghc" $ do
        initCradle "src/MyLib.hs"
        assertCradle isCabalCradle
        loadRuntimeGhcLibDir
        assertLibDirVersionIs extraGhcVersion
        loadRuntimeGhcVersion
        assertGhcVersionIs extraGhcVersion
    ]
  , testGroup "Cabal cabalProject"
    [ testCaseSteps "cabal-with-project, options propagated" $ runTestEnv "cabal-with-project" $ do
        opts <- cabalLoadOptions "src/MyLib.hs"
        liftIO $ do
          "-O2" `elem` componentOptions opts
            @? "Options must contain '-O2'"
    , testCaseSteps "cabal-with-project, load" $ runTestEnv "cabal-with-project" $ do
        testDirectoryM isCabalCradle "src/MyLib.hs"
    , testCaseSteps "multi-cabal-with-project, options propagated" $ runTestEnv "multi-cabal-with-project" $ do
        optsAppA <- cabalLoadOptions "appA/src/Lib.hs"
        liftIO $ do
          "-O2" `elem` componentOptions optsAppA
            @? "Options must contain '-O2'"
        optsAppB <- cabalLoadOptions "appB/src/Lib.hs"
        liftIO $ do
          "-O2" `notElem` componentOptions optsAppB
            @? "Options must not contain '-O2'"
    , testCaseSteps "multi-cabal-with-project, load" $ runTestEnv "multi-cabal-with-project" $ do
        testDirectoryM isCabalCradle "appB/src/Lib.hs"
        testDirectoryM isCabalCradle "appB/src/Lib.hs"
    , testGroupWithDependency extraGhcDep
      [ testCaseSteps "Honours extra ghc setting" $ runTestEnv "cabal-with-ghc-and-project" $ do
          initCradle "src/MyLib.hs"
          assertCradle isCabalCradle
          loadRuntimeGhcLibDir
          assertLibDirVersionIs extraGhcVersion
          loadRuntimeGhcVersion
          assertGhcVersionIs extraGhcVersion
      ]
    ]
  ]
  where
    cabalAttemptLoad :: FilePath -> TestM ()
    cabalAttemptLoad fp = do
      initCradle fp
      assertCradle isCabalCradle
      loadComponentOptions fp

    cabalLoadOptions :: FilePath -> TestM ComponentOptions
    cabalLoadOptions fp = do
      initCradle fp
      assertCradle isCabalCradle
      loadComponentOptions fp
      assertLoadSuccess

stackTestCases :: [TestTree]
stackTestCases =
  [ expectFailBecause "stack repl does not fail on an invalid cabal file" $
      testCaseSteps "failing-stack" $ runTestEnv "./failing-stack" $ do
        stackAttemptLoad "src/Lib.hs"
        assertCradleError $ \CradleError {..} -> do
            cradleErrorExitCode @?= ExitFailure 1
            cradleErrorDependencies `shouldMatchList` ["failing-stack.cabal", "stack.yaml", "package.yaml"]
  , testCaseSteps "simple-stack" $ runTestEnv "./simple-stack" $ do
      testDirectoryM isStackCradle "B.hs"
  , testCaseSteps "multi-stack" $ runTestEnv "./multi-stack" $ do {- tests if both components can be loaded -}
      testDirectoryM isStackCradle "app/Main.hs"
      testDirectoryM isStackCradle "src/Lib.hs"
  , testCaseSteps "nested-stack" $ runTestEnv "./nested-stack" $ do
      stackAttemptLoad "sub-comp/Lib.hs"
      assertComponentOptions $ \opts ->
        componentDependencies opts `shouldMatchList` ["sub-comp" </> "sub-comp.cabal", "sub-comp" </> "package.yaml", "stack.yaml"]
  , testCaseSteps "nested-stack2" $ runTestEnv "./nested-stack" $ do
      stackAttemptLoad "MyLib.hs"
      assertComponentOptions $ \opts ->
        componentDependencies opts `shouldMatchList` ["nested-stack.cabal", "package.yaml", "stack.yaml"]
  , testCaseSteps "stack-with-yaml" $ runTestEnv "./stack-with-yaml" $ do
      {- tests if both components can be loaded -}
      testDirectoryM isStackCradle "app/Main.hs"
      testDirectoryM isStackCradle "src/Lib.hs"
  , testCaseSteps "multi-stack-with-yaml" $ runTestEnv "./multi-stack-with-yaml" $ do
      {- tests if both components can be loaded -}
      testDirectoryM isStackCradle "appA/src/Lib.hs"
      testDirectoryM isStackCradle "appB/src/Lib.hs"
  ,
    -- Test for special characters in the path for parsing of the ghci-scripts.
    -- Issue https://github.com/mpickering/hie-bios/issues/162
    testCaseSteps "space stack" $ runTestEnv "./space stack" $ do
      testDirectoryM isStackCradle "A.hs"
      testDirectoryM isStackCradle "B.hs"
  , testGroup "Implicit cradle tests"
      [ testCaseSteps "implicit-stack" $ runTestEnv "./implicit-stack" $
          testImplicitDirectoryM isStackCradle "Main.hs"
      , testCaseSteps "implicit-stack-multi" $ runTestEnv "./implicit-stack-multi" $ do
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
  [ testCaseSteps "simple-direct" $ runTestEnv  "./simple-direct" $ do
      testDirectoryM isDirectCradle "B.hs"
  , testCaseSteps "multi-direct" $ runTestEnv "./multi-direct" $ do
      {- tests if both components can be loaded -}
      testDirectoryM isMultiCradle "A.hs"
      testDirectoryM isMultiCradle "B.hs"
  ]

-- ------------------------------------------------------------------
-- Unit-test Helper functions
-- ------------------------------------------------------------------

shouldMatchList :: (Show a, Ord a) => [a] -> [a] -> Assertion
shouldMatchList xs ys = sort xs @?= sort ys

infix 1 `shouldMatchList`

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
#if (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(9,6,2,0)))
  "nightly-2023-08-22" -- GHC 9.6.2
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(9,4,4,0)))
  "lts-21.8" -- GHC 9.4.6
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(9,2,7,0)))
  "lts-20.26" -- GHC 9.2.8
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(9,2,1,0)))
  "lts-20.11" -- GHC 9.2.5
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(9,0,1,0)))
  "lts-19.33" -- GHC 9.0.2
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,10,7,0)))
  "lts-18.28" -- GHC 8.10.7
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,10,1,0)))
  "lts-18.6" -- GHC 8.10.4
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,8,1,0)))
  "lts-16.31" -- GHC 8.8.4
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,6,5,0)))
  "lts-14.27" -- GHC 8.6.5
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,6,4,0)))
  "lts-13.19" -- GHC 8.6.4
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

-- | The ingredient that provides the "ignore missing run-time dependencies" functionality
ignoreToolTests :: Tasty.Ingredient
ignoreToolTests = Tasty.TestManager [Tasty.Option (Proxy :: Proxy IgnoreToolDeps)] $
  \_opts _tree -> Nothing

-- ------------------------------------------------------------------
-- Ignore test group if built with GHC 9.2.1 until GHC 9.2.4
-- ------------------------------------------------------------------

ignoreOnUnsupportedGhc :: TestTree -> TestTree
ignoreOnUnsupportedGhc tt =
#if (defined(MIN_VERSION_GLASGOW_HASKELL) && MIN_VERSION_GLASGOW_HASKELL(9,2,1,0) && !MIN_VERSION_GLASGOW_HASKELL(9,2,4,0))
  ignoreTestBecause "Not supported on GHC >= 9.2.1 && < 9.2.4"
#endif
  tt

