module HIE.Bios.Ghc.Check (
    checkSyntax
  , check
  , expandTemplate
  , expand
  ) where

import DynFlags (dopt_set, DumpFlag(Opt_D_dump_splices))
import GHC (Ghc, DynFlags(..), GhcMonad)
import Exception

import HIE.Bios.Ghc.Api
import HIE.Bios.Ghc.Logger
import qualified HIE.Bios.Log as Log
import HIE.Bios.Types
import HIE.Bios.Ghc.Load
import Control.Monad.IO.Class

----------------------------------------------------------------

-- | Checking syntax of a target file using GHC.
--   Warnings and errors are returned.
checkSyntax :: Options
            -> Cradle
            -> [FilePath]  -- ^ The target files.
            -> IO String
checkSyntax _   _      []    = return ""
checkSyntax opt cradle files = withGhcT $ do
    Log.debugm $ "Cradle: " ++ show cradle
    res <- initializeFlagsWithCradle (head files) cradle
    case res of
      CradleSuccess ini -> do
        ini
        either id id <$> check opt files
      CradleFail ce -> liftIO $ throwIO ce
      CradleNone -> return "No cradle"


  where
    {-
    sessionName = case files of
      [file] -> file
      _      -> "MultipleFiles"
      -}

----------------------------------------------------------------

-- | Checking syntax of a target file using GHC.
--   Warnings and errors are returned.
check :: (GhcMonad m)
      => Options
      -> [FilePath]  -- ^ The target files.
      -> m (Either String String)
check opt fileNames = withLogger opt setAllWarningFlags $ setTargetFiles (map dup fileNames)

dup :: a -> (a, a)
dup x = (x, x)

----------------------------------------------------------------

-- | Expanding Haskell Template.
expandTemplate :: Options
               -> Cradle
               -> [FilePath]  -- ^ The target files.
               -> IO String
expandTemplate _   _      []    = return ""
expandTemplate opt cradle files = withGHC sessionName $ do
    res <- initializeFlagsWithCradle (head files) cradle
    case res of
      CradleSuccess ini -> do
        ini
        either id id <$> expand opt files
      CradleNone -> return "<no cradle>"
      CradleFail err -> liftIO $ throwIO err
  where
    sessionName = case files of
      [file] -> file
      _      -> "MultipleFiles"

----------------------------------------------------------------

-- | Expanding Haskell Template.
expand :: Options
      -> [FilePath]  -- ^ The target files.
      -> Ghc (Either String String)
expand opt fileNames = withLogger opt (setDumpSplices . setNoWarningFlags) $ setTargetFiles (map dup fileNames)

setDumpSplices :: DynFlags -> DynFlags
setDumpSplices dflag = dopt_set dflag Opt_D_dump_splices
