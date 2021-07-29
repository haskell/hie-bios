{-# LANGUAGE ScopedTypeVariables, CPP #-}
-- | These functions are for conveniently implementing the simple CLI
module HIE.Bios.Ghc.Api (
    initializeFlagsWithCradle
  , initializeFlagsWithCradleWithMessage
  , G.SuccessFlag(..)
  , withDynFlags
  ) where

import GHC (LoadHowMuch(..), DynFlags, GhcMonad)
import qualified GHC as G

#if __GLASGOW_HASKELL__ >= 900
import qualified GHC.Driver.Main as G
import qualified GHC.Driver.Make as G
#else
import qualified HscMain as G
import qualified GhcMake as G
#endif

import qualified HIE.Bios.Ghc.Gap as Gap
import Control.Monad (void)
import Control.Monad.IO.Class
import HIE.Bios.Types
import HIE.Bios.Environment
import HIE.Bios.Flags

----------------------------------------------------------------

-- | Initialize a GHC session by loading a given file into a given cradle.
initializeFlagsWithCradle ::
    GhcMonad m
    => FilePath -- ^ The file we are loading the 'Cradle' because of
    -> Cradle a   -- ^ The cradle we want to load
    -> m (CradleLoadResult (m G.SuccessFlag, ComponentOptions))
initializeFlagsWithCradle = initializeFlagsWithCradleWithMessage (Just Gap.batchMsg)

-- | The same as 'initializeFlagsWithCradle' but with an additional argument to control
-- how the loading progress messages are displayed to the user. In @haskell-ide-engine@
-- the module loading progress is displayed in the UI by using a progress notification.
initializeFlagsWithCradleWithMessage ::
  GhcMonad m
  => Maybe G.Messager
  -> FilePath -- ^ The file we are loading the 'Cradle' because of
  -> Cradle a  -- ^ The cradle we want to load
  -> m (CradleLoadResult (m G.SuccessFlag, ComponentOptions)) -- ^ Whether we actually loaded the cradle or not.
initializeFlagsWithCradleWithMessage msg fp cradle =
    fmap (initSessionWithMessage msg) <$> liftIO (getCompilerOptions fp cradle)

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
withDynFlags setFlag body = Gap.bracket setup teardown (\_ -> body)
  where
    setup = do
        dflag <- G.getSessionDynFlags
        void $ G.setSessionDynFlags (setFlag dflag)
        return dflag
    teardown = void . G.setSessionDynFlags

----------------------------------------------------------------
