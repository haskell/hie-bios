{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, CPP #-}
-- | These functions are for conveniently implementing the simple CLI
module HIE.Bios.Ghc.Api (
    initializeFlagsWithCradle
  , initializeFlagsWithCradleWithMessage
  , G.SuccessFlag(..)
  , withDynFlags
  ) where

import CoreMonad (liftIO)
import GHC (LoadHowMuch(..), GhcMonad)
import DynFlags

import qualified GHC as G
import qualified HscMain as G
import qualified GhcMake as G

import Control.Monad (join, forM, void)
import HIE.Bios.Types
import HIE.Bios.Environment
import HIE.Bios.Flags

----------------------------------------------------------------

-- | Initialize a GHC session by loading a given file into a given cradle.
initializeFlagsWithCradle ::
       FilePath -- ^ The file we are loading the 'Cradle' because of
    -> Cradle a   -- ^ The cradle we want to load
    -> ((G.Ghc G.SuccessFlag, ComponentOptions) -> G.Ghc b)
    -> IO (CradleLoadResult b)
initializeFlagsWithCradle = initializeFlagsWithCradleWithMessage (Just G.batchMsg)

-- | The same as 'initializeFlagsWithCradle' but with an additional argument to control
-- how the loading progress messages are displayed to the user. In @haskell-ide-engine@
-- the module loading progress is displayed in the UI by using a progress notification.
initializeFlagsWithCradleWithMessage ::
     Maybe G.Messager
  -> FilePath -- ^ The file we are loading the 'Cradle' because of
  -> Cradle a  -- ^ The cradle we want to load
  -> ((G.Ghc G.SuccessFlag, ComponentOptions) -> G.Ghc b)
  -> IO (CradleLoadResult b)
initializeFlagsWithCradleWithMessage msg fp cradle cont = do
  componentLoadResult <- liftIO (getCompilerOptions fp cradle)
  fmap join $ forM componentLoadResult $ \opts -> do
    libDirResult <- getRuntimeGhcLibDir opts
    forM libDirResult $ \libDir -> do
      let res = initSessionWithMessage msg opts
      G.runGhc (Just libDir) (cont res)

-- | Actually perform the initialisation of the session. Initialising the session corresponds to
-- parsing the command line flags, setting the targets for the session and then attempting to load
-- all the targets.
initSessionWithMessage :: (GhcMonad m)
            => Maybe G.Messager
            -> ComponentOptions
            -> (m G.SuccessFlag, ComponentOptions)
initSessionWithMessage msg compOpts = (do
    targets <- initSession compOpts
    G.setTargets targets
    -- Get the module graph using the function `getModuleGraph`
    mod_graph <- G.depanal [] True
    G.load' LoadAllTargets msg mod_graph, compOpts)

----------------------------------------------------------------

withDynFlags ::
  (GhcMonad m)
  => (DynFlags -> DynFlags) -> m a -> m a
withDynFlags setFlag body = G.gbracket setup teardown (\_ -> body)
  where
    setup = do
        dflag <- G.getSessionDynFlags
        void $ G.setSessionDynFlags (setFlag dflag)
        return dflag
    teardown = void . G.setSessionDynFlags

----------------------------------------------------------------
