module Main where

import Test.Tasty
import Test.Tasty.HUnit
import HIE.Bios
import HIE.Bios.Ghc.Api
import Control.Monad.IO.Class
import System.Directory
import BasicTypes

configDir :: FilePath
configDir = "tests/configs"

main :: IO ()
main = defaultMain $ testGroup "Loading tests" [
  testCaseSteps "simple-cabal" $ testDirectory "./tests/projects/simple-cabal/B.hs"
  , testCaseSteps "simple-stack" $ testDirectory "./tests/projects/simple-stack/B.hs"
  , testCaseSteps "simple-direct" $ testDirectory "./tests/projects/simple-direct/B.hs"
  , testCaseSteps "simple-bios" $ testDirectory "./tests/projects/simple-bios/B.hs"
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
      res <- initializeFlagsWithCradleWithMessage (Just (\_ n _ _ -> step (show n))) fp crd
      case res of
        CradleSuccess ini -> do
          liftIO (step "Initial module load")
          sf <- ini
          case sf of
            Succeeded -> return ()
            Failed -> error "Module loading failed"
        CradleNone -> error "None"
        CradleFail (CradleError _ex stde) -> error (unlines stde)