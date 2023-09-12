{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
module HIE.Bios.Ghc.Check (
    checkSyntax
  , check
  ) where

import GHC (GhcMonad)
import qualified GHC as G


import Control.Exception
import Control.Monad.IO.Class
import Colog.Core (LogAction (..), WithSeverity (..), Severity (..), (<&), cmap)
import Data.Text.Prettyprint.Doc

import HIE.Bios.Ghc.Api
import HIE.Bios.Ghc.Logger
import HIE.Bios.Types hiding (Log (..))
import qualified HIE.Bios.Types as T
import qualified HIE.Bios.Ghc.Load as Load
import HIE.Bios.Environment


data Log =
  LoadLog Load.Log
  | LogAny T.Log
  | forall a . Show a => LogCradle (Cradle a)

instance Pretty Log where
  pretty (LoadLog l) = pretty l
  pretty (LogAny l) = pretty l
  pretty (LogCradle c) = "Cradle:" <+> viaShow c

----------------------------------------------------------------

-- | Checking syntax of a target file using GHC.
--   Warnings and errors are returned.
checkSyntax :: Show a
            => LogAction IO (WithSeverity Log)
            -> Cradle a
            -> [FilePath]  -- ^ The target files.
            -> IO String
checkSyntax _           _      []    = return ""
checkSyntax checkLogger cradle files = do
    libDirRes <- getRuntimeGhcLibDir cradle
    handleRes libDirRes $ \libDir ->
      G.runGhcT (Just libDir) $ do
        liftIO $ checkLogger <& LogCradle cradle `WithSeverity` Info
        res <- initializeFlagsWithCradle (head files) cradle
        handleRes res $ \(ini, _) -> do
          _sf <- ini
          either id id <$> check checkLogger files
  where
    handleRes (CradleSuccess x) f = f x
    handleRes (CradleFail ce) _f = liftIO $ throwIO ce
    handleRes CradleNone _f = return "None cradle"

----------------------------------------------------------------

-- | Checking syntax of a target file using GHC.
--   Warnings and errors are returned.
check :: (GhcMonad m)
      => LogAction IO (WithSeverity Log)
      -> [FilePath]  -- ^ The target files.
      -> m (Either String String)
check logger fileNames = do
  withLogger id $ Load.setTargetFiles (cmap (fmap LoadLog) logger) (map dup fileNames)

dup :: a -> (a, a)
dup x = (x, x)

