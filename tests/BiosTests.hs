module Main where

import Test.Tasty
import Test.Tasty.HUnit
import HIE.Bios
import HIE.Bios.Ghc.Api
import Control.Monad.IO.Class
import Control.Monad ( unless )
import System.Directory
import System.FilePath ( makeRelative )
import BasicTypes

main :: IO ()
main = defaultMain $
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
    , testGroup "Loading tests" [
      testCaseSteps "simple-cabal" $ testDirectory "./tests/projects/simple-cabal/B.hs"
      , testCaseSteps "simple-stack" $ testDirectory "./tests/projects/simple-stack/B.hs"
      , testCaseSteps "simple-direct" $ testDirectory "./tests/projects/simple-direct/B.hs"
      , testCaseSteps "simple-bios" $ testDirectory "./tests/projects/simple-bios/B.hs"
      , testCaseSteps "multi-cabal" {- tests if both components can be loaded -}
                    $  testDirectory "./tests/projects/multi-cabal/app/Main.hs"
                    >> testDirectory "./tests/projects/multi-cabal/src/Lib.hs"
      , testCaseSteps "multi-stack" {- tests if both components can be loaded -}
                    $  testDirectory "./tests/projects/multi-stack/app/Main.hs"
                    >> testDirectory "./tests/projects/multi-stack/src/Lib.hs"
      ]
  ]



testDirectory :: FilePath -> (String -> IO ()) -> IO ()
testDirectory fp step = do
  a_fp <- canonicalizePath fp
  step "Finding Cradle"
  mcfg <- findCradle a_fp
  step "Loading Cradle"
  crd <- case mcfg of
          Just cfg -> loadCradle cfg
          Nothing -> loadImplicitCradle a_fp
  step "Initialise Flags"
  withCurrentDirectory (cradleRootDir crd) $
    withGHC' $ do
      let relFp = makeRelative (cradleRootDir crd) a_fp
      res <- initializeFlagsWithCradleWithMessage (Just (\_ n _ _ -> step (show n))) relFp crd
      case res of
        CradleSuccess ini -> do
          liftIO (step "Initial module load")
          sf <- ini
          case sf of
            Succeeded -> return ()
            Failed -> error "Module loading failed"
        CradleNone -> error "None"
        CradleFail (CradleError _ex stde) -> error (unlines stde)

findCradleForModule :: FilePath -> Maybe FilePath -> (String -> IO ()) -> IO ()
findCradleForModule fp expected' step = do
  expected <- maybe (return Nothing) (fmap Just . canonicalizePath) expected'
  a_fp <- canonicalizePath fp
  step "Finding cradle"
  mcfg <- findCradle a_fp
  unless (mcfg == expected)
    $  error
    $  "Expected cradle: "
    ++ show expected
    ++ ", Actual: "
    ++ show mcfg
