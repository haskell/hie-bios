{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Hspec.Expectations
#if __GLASGOW_HASKELL__ < 810
import Test.Tasty.ExpectedFailure
#endif
import qualified GHC as G
import HIE.Bios
import HIE.Bios.Ghc.Api
import HIE.Bios.Ghc.Load
import HIE.Bios.Cradle
import HIE.Bios.Environment
import HIE.Bios.Types
import Control.Monad.IO.Class
import Control.Monad ( forM_, unless )
import Data.Void
import System.Directory
import System.FilePath ( makeRelative, (</>) )
import System.Info.Extra ( isWindows )
import System.IO.Temp
import System.Exit (ExitCode(ExitFailure))

main :: IO ()
main = do
  writeStackYamlFiles
  defaultMain $
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
      , testGroup "Loading tests"
        $ linuxExlusiveTestCases
        ++
           [ testCaseSteps "failing-cabal" $ testDirectoryFail isCabalCradle "./tests/projects/failing-cabal" "MyLib.hs"
              (\CradleError {..} -> do
                  cradleErrorExitCode `shouldBe` ExitFailure 1
                  cradleErrorDependencies `shouldMatchList` ["failing-cabal.cabal", "cabal.project", "cabal.project.local"])
           , testCaseSteps "failing-bios" $ testDirectoryFail isBiosCradle "./tests/projects/failing-bios" "B.hs"
              (\CradleError {..} -> do
                  cradleErrorExitCode `shouldBe` ExitFailure 1
                  cradleErrorDependencies `shouldMatchList` ["hie.yaml"])
           , testCaseSteps "simple-bios-shell" $ testDirectory isBiosCradle "./tests/projects/simple-bios-shell" "B.hs"
           , testCaseSteps "simple-cabal" $ testDirectory isCabalCradle "./tests/projects/simple-cabal" "B.hs"
           , testCaseSteps "simple-direct" $ testDirectory isDirectCradle "./tests/projects/simple-direct" "B.hs"
           , testCaseSteps "nested-cabal" $ testLoadCradleDependencies isCabalCradle "./tests/projects/nested-cabal" "sub-comp/Lib.hs"
              (\deps -> deps `shouldMatchList` ["sub-comp" </> "sub-comp.cabal", "cabal.project", "cabal.project.local"]
              )
           , testCaseSteps "nested-cabal2" $ testLoadCradleDependencies isCabalCradle "./tests/projects/nested-cabal" "MyLib.hs"
              (\deps -> deps `shouldMatchList` ["nested-cabal.cabal", "cabal.project", "cabal.project.local"]
              )
           , testCaseSteps "multi-direct" {- tests if both components can be loaded -}
                         $  testDirectory isMultiCradle "./tests/projects/multi-direct" "A.hs"
                         >> testDirectory isMultiCradle "./tests/projects/multi-direct" "B.hs"
           , testCaseSteps "multi-cabal" {- tests if both components can be loaded -}
                         $  testDirectory isCabalCradle "./tests/projects/multi-cabal" "app/Main.hs"
                         >> testDirectory isCabalCradle "./tests/projects/multi-cabal" "src/Lib.hs"
           , testCaseSteps "monorepo-cabal" {- issue https://github.com/mpickering/hie-bios/issues/200 -}
                         $  testDirectory isCabalCradle "./tests/projects/monorepo-cabal" "A/Main.hs"
                         >> testDirectory isCabalCradle "./tests/projects/monorepo-cabal" "B/MyLib.hs"
           ]
-- TODO: Remove once stack and ghc-8.10.1 play well
-- https://github.com/bubba/hie-bios/runs/811271872?check_suite_focus=true
#if __GLASGOW_HASKELL__ < 810
       ++ [ expectFailBecause "stack repl does not fail on an invalid cabal file" $
              testCaseSteps "failing-stack" $ testDirectoryFail isStackCradle "./tests/projects/failing-stack" "src/Lib.hs"
                (\CradleError {..} -> do
                    cradleErrorExitCode `shouldBe` ExitFailure 1
                    cradleErrorDependencies `shouldMatchList` ["failing-stack.cabal", "stack.yaml", "package.yaml"])
          , testCaseSteps "simple-stack" $ testDirectory isStackCradle "./tests/projects/simple-stack" "B.hs"
          , testCaseSteps "multi-stack" {- tests if both components can be loaded -}
                        $  testDirectory isStackCradle "./tests/projects/multi-stack" "app/Main.hs"
                        >> testDirectory isStackCradle "./tests/projects/multi-stack" "src/Lib.hs"
          , expectFailBecause "stack repl set the component directory to the root directory" $
              testCaseSteps "nested-stack" $ testLoadCradleDependencies isStackCradle "./tests/projects/nested-stack" "sub-comp/Lib.hs"
                (\deps -> deps `shouldMatchList`
                  ["sub-comp" </> "sub-comp.cabal", "sub-comp" </> "package.yaml", "stack.yaml"]
                )
          , testCaseSteps "nested-stack2" $ testLoadCradleDependencies isStackCradle "./tests/projects/nested-stack" "MyLib.hs"
              (\deps -> deps `shouldMatchList` ["nested-stack.cabal", "package.yaml", "stack.yaml"]
              )
          ,
          -- Test for special characters in the path for parsing of the ghci-scripts.
          -- Issue https://github.com/mpickering/hie-bios/issues/162
          testCaseSteps "space stack"
                        $  testDirectory isStackCradle "./tests/projects/space stack" "A.hs"
                        >> testDirectory isStackCradle "./tests/projects/space stack" "B.hs"
          ]
#endif
      , testGroup "Implicit cradle tests" $
        [ testCaseSteps "implicit-cabal" $
            testImplicitCradle "./tests/projects/implicit-cabal" "Main.hs" Cabal
        , testCaseSteps "implicit-cabal-no-project" $
            testImplicitCradle "./tests/projects/implicit-cabal-no-project" "Main.hs" Cabal
        , testCaseSteps "implicit-cabal-deep-project" $
            testImplicitCradle "./tests/projects/implicit-cabal-deep-project" "foo/Main.hs" Cabal
-- TODO: same here
#if __GLASGOW_HASKELL__ < 810
        , testCaseSteps "implicit-stack" $
            testImplicitCradle "./tests/projects/implicit-stack" "Main.hs" Stack
        , testCaseSteps "implicit-stack-multi"
            $ testImplicitCradle "./tests/projects/implicit-stack-multi" "Main.hs" Stack
            >> testImplicitCradle "./tests/projects/implicit-stack-multi" "other-package/Main.hs" Stack
#endif
        ]
      ]

linuxExlusiveTestCases :: [TestTree]
linuxExlusiveTestCases =
  [ testCaseSteps "simple-bios" $ testDirectory isBiosCradle "./tests/projects/simple-bios" "B.hs" | not isWindows ]

testDirectory :: (Cradle Void -> Bool) -> FilePath -> FilePath -> (String -> IO ()) -> IO ()
testDirectory cradlePred rootDir file step =
  -- We need to copy over the directory to somewhere outside the source tree
  -- when we test, since the cabal.project/stack.yaml/hie.yaml file in the root
  -- of this repository interferes with the test cradles!
  withTempCopy rootDir $ \rootDir' -> do
    fp <- canonicalizePath (rootDir' </> file)
    crd <- initialiseCradle cradlePred fp step
    step "Get runtime GHC library directory"
    testGetGhcLibDir crd
    step "Get runtime GHC version"
    testGetGhcVersion crd
    step "Initialise Flags"
    testLoadFile crd fp step

-- | Here we are testing that the cradle's method of obtaining the ghcLibDir
-- always works.
testGetGhcLibDir :: Cradle a -> IO ()
testGetGhcLibDir crd = do
  libDirRes <- getRuntimeGhcLibDir crd
  libDirRes `shouldSatisfy` isSuccess
  where isSuccess (CradleSuccess _) = True
        isSuccess _ = False

-- | Here we are testing that the cradle's method of getting the runtime ghc
-- version is correct - which while testing, should be the version that we have
-- built the tests with. This will fail if you compiled the tests with a ghc
-- that doesn't equal the ghc on your path though :(
testGetGhcVersion :: Cradle a -> IO ()
testGetGhcVersion crd =
  getRuntimeGhcVersion crd `shouldReturn` CradleSuccess VERSION_ghc

testDirectoryFail :: (Cradle Void -> Bool) -> FilePath -> FilePath -> (CradleError -> Expectation) -> (String -> IO ()) -> IO ()
testDirectoryFail cradlePred rootDir file cradleFailPred step = do
  withTempCopy rootDir $ \rootDir' -> do
    fp <- canonicalizePath (rootDir' </> file)
    crd <- initialiseCradle cradlePred fp step
    step "Initialise Flags"
    testLoadFileCradleFail crd fp cradleFailPred step

initialiseCradle :: (Cradle Void -> Bool) -> FilePath -> (String -> IO ()) -> IO (Cradle Void)
initialiseCradle cradlePred a_fp step = do
  step $ "Finding Cradle for: " ++ a_fp
  mcfg <- findCradle a_fp
  step $ "Loading Cradle: " ++ show mcfg
  crd <- case mcfg of
          Just cfg -> loadCradle cfg
          Nothing -> loadImplicitCradle a_fp
  crd `shouldSatisfy` cradlePred
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
            Failed -> liftIO $ expectationFailure "Module loading failed"

testLoadFileCradleFail :: Cradle a -> FilePath -> (CradleError -> Expectation) -> (String -> IO ()) -> IO ()
testLoadFileCradleFail crd a_fp cradleErrorExpectation step = do
  -- don't spin up a ghc session, just run the opts program manually since
  -- we're not guaranteed to be able to get the ghc libdir if the cradle is
  -- failing
  withCurrentDirectory (cradleRootDir crd) $ do
    let relFp = makeRelative (cradleRootDir crd) a_fp
    res <- runCradle (cradleOptsProg crd) (step . show) relFp
    case res of
      CradleSuccess _ -> liftIO $ expectationFailure "Cradle loaded successfully"
      CradleNone -> liftIO $ expectationFailure "Unexpected none-Cradle"
      CradleFail crdlFail -> liftIO $ cradleErrorExpectation crdlFail

testLoadCradleDependencies :: (Cradle Void -> Bool) -> FilePath -> FilePath -> ([FilePath] -> Expectation) -> (String -> IO ()) -> IO ()
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
handleCradleResult CradleNone _ = liftIO $ expectationFailure "Unexpected none-Cradle"
handleCradleResult (CradleFail (CradleError _deps _ex stde)) _ =
  liftIO $ expectationFailure ("Unexpected cradle fail" ++ unlines stde)

findCradleForModule :: FilePath -> Maybe FilePath -> (String -> IO ()) -> IO ()
findCradleForModule fp expected' step = do
  expected <- maybe (return Nothing) (fmap Just . canonicalizePath) expected'
  a_fp <- canonicalizePath fp
  step "Finding cradle"
  findCradle a_fp `shouldReturn` expected

testImplicitCradle :: FilePath -> FilePath -> ActionName Void -> (String -> IO ()) -> IO ()
testImplicitCradle rootDir file expectedActionName step =
  withTempCopy rootDir $ \rootDir' -> do
    fp <- makeAbsolute (rootDir' </> file)
    step "Inferring implicit cradle"
    crd <- loadImplicitCradle fp :: IO (Cradle Void)

    actionName (cradleOptsProg crd) `shouldBe` expectedActionName

    expectedCradleRootDir <- makeAbsolute rootDir'
    cradleRootDir crd `shouldBe` expectedCradleRootDir

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
      

writeStackYamlFiles :: IO ()
writeStackYamlFiles =
  forM_ stackProjects $ \(proj, pkgs) ->
    writeFile (proj </> "stack.yaml") (stackYaml stackYamlResolver pkgs)

stackProjects :: [(FilePath, [FilePath])]
stackProjects =
  [ ("tests" </> "projects" </> "multi-stack", ["."])
  , ("tests" </> "projects" </> "failing-stack", ["."])
  , ("tests" </> "projects" </> "simple-stack", ["."])
  , ("tests" </> "projects" </> "nested-stack", [".", "./sub-comp"])
  , ("tests" </> "projects" </> "space stack", ["."])
  , ("tests" </> "projects" </> "implicit-stack", ["."])
  , ("tests" </> "projects" </> "implicit-stack-multi", ["."])
  ]

stackYaml :: String -> [FilePath] -> String
stackYaml resolver pkgs = unlines
  $ ["resolver: " ++ resolver, "packages:"]
  ++ map ("- " ++) pkgs

stackYamlResolver :: String
stackYamlResolver =
#if (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,10,1,0)))
  "nightly-2020-06-25"
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,8,1,0)))
  "lts-15.10"
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,6,5,0)))
  "lts-14.17"
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,6,4,0)))
  "lts-13.19"
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,4,4,0)))
  "lts-12.26"
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,4,3,0)))
  "lts-12.26"
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,2,2,0)))
  "lts-11.22"
#endif
