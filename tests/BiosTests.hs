{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Hspec.Expectations
import HIE.Bios
import HIE.Bios.Ghc.Api
import HIE.Bios.Ghc.Load
import HIE.Bios.Cradle
import HIE.Bios.Types
import Control.Monad.IO.Class
import Control.Monad ( forM_ )
import Data.Void
import System.Directory
import System.FilePath ( makeRelative, (</>) )
import System.Info.Extra ( isWindows )

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
           [ testCaseSteps "simple-bios-shell" $ testDirectory isBiosCradle "./tests/projects/simple-bios-shell/B.hs"
           , testCaseSteps "simple-cabal" $ testDirectory isCabalCradle "./tests/projects/simple-cabal/B.hs"
           , testCaseSteps "simple-direct" $ testDirectory isDirectCradle "./tests/projects/simple-direct/B.hs"
           , testCaseSteps "multi-direct" {- tests if both components can be loaded -}
                         $  testDirectory isMultiCradle "./tests/projects/multi-direct/A.hs"
                         >> testDirectory isMultiCradle "./tests/projects/multi-direct/B.hs"
           , testCaseSteps "multi-cabal" {- tests if both components can be loaded -}
                         $  testDirectory isCabalCradle "./tests/projects/multi-cabal/app/Main.hs"
                         >> testDirectory isCabalCradle "./tests/projects/multi-cabal/src/Lib.hs"
           ]
-- TODO: Remove once there's a stackage snapshot for ghc 8.10
#if __GLASGOW_HASKELL__ < 810
       ++ [ testCaseSteps "simple-stack" $ testDirectory isStackCradle "./tests/projects/simple-stack/B.hs"
          , testCaseSteps "multi-stack" {- tests if both components can be loaded -}
                        $  testDirectory isStackCradle "./tests/projects/multi-stack/app/Main.hs"
                        >> testDirectory isStackCradle "./tests/projects/multi-stack/src/Lib.hs"
          ,
          -- Test for special characters in the path for parsing of the ghci-scripts.
          -- Issue https://github.com/mpickering/hie-bios/issues/162
          testCaseSteps "space stack"
                        $  testDirectory isStackCradle "./tests/projects/space stack/A.hs"
                        >> testDirectory isStackCradle "./tests/projects/space stack/B.hs"
          ]
#endif
      , testGroup "Implicit cradle tests" $
        [ testCaseSteps "implicit-cabal" $ testImplicitCradle "./tests/projects/implicit-cabal/Main.hs" Cabal
-- TODO: Remove once there's a stackage snapshot for ghc 8.10
#if __GLASGOW_HASKELL__ < 810
        , testCaseSteps "implicit-stack" $ testImplicitCradle "./tests/projects/implicit-stack/Main.hs" Stack
        , testCaseSteps "implicit-stack-multi"
            $ testImplicitCradle "./tests/projects/implicit-stack-multi/Main.hs" Stack
            >> testImplicitCradle "./tests/projects/implicit-stack-multi/other-package/Main.hs" Stack
#endif
        ]
      ]

linuxExlusiveTestCases :: [TestTree]
linuxExlusiveTestCases =
  [ testCaseSteps "simple-bios" $ testDirectory isBiosCradle "./tests/projects/simple-bios/B.hs" | not isWindows ]

testDirectory :: (Cradle Void -> Bool) -> FilePath -> (String -> IO ()) -> IO ()
testDirectory cradlePred fp step = do
  a_fp <- canonicalizePath fp
  step $ "Finding Cradle for: " ++ a_fp
  mcfg <- findCradle a_fp
  step $ "Loading Cradle: " ++ show mcfg
  crd <- case mcfg of
          Just cfg -> loadCradle cfg
          Nothing -> loadImplicitCradle a_fp
  crd `shouldSatisfy` cradlePred
  step "Initialise Flags"
  testLoadFile crd a_fp step

testLoadFile :: Cradle a -> FilePath -> (String -> IO ()) -> IO ()
testLoadFile crd fp step = do
  a_fp <- canonicalizePath fp
  withCurrentDirectory (cradleRootDir crd) $
    withGHC' $ do
      let relFp = makeRelative (cradleRootDir crd) a_fp
      res <- initializeFlagsWithCradleWithMessage (Just (\_ n _ _ -> step (show n))) relFp crd
      case res of
        CradleSuccess (ini, _) -> do
          liftIO (step "Initial module load")
          sf <- ini
          case sf of
            -- Test resetting the targets
            Succeeded -> setTargetFilesWithMessage (Just (\_ n _ _ -> step (show n))) [(a_fp, a_fp)]
            Failed -> liftIO $ expectationFailure "Module loading failed"
        CradleNone -> liftIO $ expectationFailure "None"
        CradleFail (CradleError _deps _ex stde) -> liftIO $ expectationFailure (unlines stde)

testLoadFileCradleFail :: Cradle a -> FilePath -> (CradleError -> Bool) -> (String -> IO ()) -> IO ()
testLoadFileCradleFail crd fp cradleFailPred step = do
  a_fp <- canonicalizePath fp
  withCurrentDirectory (cradleRootDir crd) $
    withGHC' $ do
      let relFp = makeRelative (cradleRootDir crd) a_fp
      res <- initializeFlagsWithCradleWithMessage (Just (\_ n _ _ -> step (show n))) relFp crd
      case res of
        CradleSuccess _ -> liftIO $ expectationFailure "Cradle loaded successfully"
        CradleFail crdlFail -> liftIO $ crdlFail `shouldSatisfy` cradleFailPred

findCradleForModule :: FilePath -> Maybe FilePath -> (String -> IO ()) -> IO ()
findCradleForModule fp expected' step = do
  expected <- maybe (return Nothing) (fmap Just . canonicalizePath) expected'
  a_fp <- canonicalizePath fp
  step "Finding cradle"
  findCradle a_fp `shouldReturn` expected

testImplicitCradle :: FilePath -> ActionName Void -> (String -> IO ()) -> IO ()
testImplicitCradle fp' expectedActionName step = do
  fp <- canonicalizePath fp'
  step "Inferring implicit cradle"
  crd <- loadImplicitCradle fp :: IO (Cradle Void)
  actionName (cradleOptsProg crd) `shouldBe` expectedActionName
  step "Initialize flags"
  testLoadFile crd fp step

writeStackYamlFiles :: IO ()
writeStackYamlFiles = do
  let yamlFile = stackYaml stackYamlResolver
  forM_ stackProjects $ \proj ->
    writeFile (proj </> "stack.yaml") yamlFile

stackProjects :: [FilePath]
stackProjects =
  [ "tests" </> "projects" </> "multi-stack"
  , "tests" </> "projects" </> "simple-stack"
  , "tests" </> "projects" </> "space stack"
  , "tests" </> "projects" </> "implicit-stack"
  , "tests" </> "projects" </> "implicit-stack-multi"
  ]

stackYaml :: String -> String
stackYaml resolver = unlines ["resolver: " ++ resolver, "packages:", "- ."]

stackYamlResolver :: String
stackYamlResolver =
#if (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,10,1,0)))
  "TODO"
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
