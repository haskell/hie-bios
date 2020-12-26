module HIE.Bios.Ghc.Check (
    checkSyntax
  , check
  ) where

import GHC (DynFlags(..), GhcMonad)
import Exception

import HIE.Bios.Environment
import HIE.Bios.Ghc.Api
import HIE.Bios.Ghc.Logger
import qualified HIE.Bios.Internal.Log as Log
import HIE.Bios.Types
import HIE.Bios.Ghc.Load
import Control.Monad.IO.Class

import System.IO.Unsafe (unsafePerformIO)
import qualified HIE.Bios.Ghc.Gap as Gap

import qualified DynFlags as G
import qualified GHC as G

----------------------------------------------------------------

-- | Checking syntax of a target file using GHC.
--   Warnings and errors are returned.
checkSyntax :: Show a
            => Cradle a
            -> [FilePath]  -- ^ The target files.
            -> IO String
checkSyntax _      []    = return ""
checkSyntax cradle files = do
        Log.debugm $ "Cradle: " ++ show cradle
        res <- initializeFlagsWithCradle (head files) cradle $ \(ini, _) -> do
          _sf <- ini
          either id id <$> check files
        handleRes res
  where
    handleRes (CradleSuccess x) = return x
    handleRes (CradleFail ce) = liftIO $ throwIO ce
    handleRes CradleNone = return "No cradle"

----------------------------------------------------------------

-- | Checking syntax of a target file using GHC.
--   Warnings and errors are returned.
check :: (GhcMonad m)
      => [FilePath]  -- ^ The target files.
      -> m (Either String String)
check fileNames = do
  libDir <- G.topDir <$> G.getDynFlags
  withLogger (setAllWarningFlags libDir) $ setTargetFiles (map dup fileNames)

dup :: a -> (a, a)
dup x = (x, x)

----------------------------------------------------------------

-- | Set 'DynFlags' equivalent to "-Wall".
setAllWarningFlags :: FilePath -> DynFlags -> DynFlags
setAllWarningFlags libDir df = df { warningFlags = allWarningFlags libDir }

{-# NOINLINE allWarningFlags #-}
allWarningFlags :: FilePath -> Gap.WarnFlags
allWarningFlags libDir = unsafePerformIO $
    G.runGhcT (Just libDir) $ do
        df <- G.getSessionDynFlags
        (df', _) <- addCmdOpts ["-Wall"] df
        return $ G.warningFlags df'
