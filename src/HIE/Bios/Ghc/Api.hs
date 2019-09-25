{-# LANGUAGE ScopedTypeVariables, RecordWildCards, CPP #-}

module HIE.Bios.Ghc.Api (
    withGHC
  , withGHC'
  , withGhcT
  , initializeFlagsWithCradle
  , initializeFlagsWithCradleWithMessage
  , getDynamicFlags
  , getSystemLibDir
  , withDynFlags
  , withCmdFlags
  , setNoWarningFlags
  , setAllWarningFlags
  , setDeferTypeErrors
  ) where

import CoreMonad (liftIO)
import Exception (ghandle, SomeException(..), ExceptionMonad(..), throwIO)
import GHC (Ghc, LoadHowMuch(..), GhcMonad, GhcT)
import DynFlags

import qualified GHC as G
import qualified MonadUtils as G
import qualified HscMain as G
import qualified GhcMake as G

import Control.Monad (void)
import System.Exit (exitSuccess)
import System.IO (hPutStr, hPrint, stderr)
import System.IO.Unsafe (unsafePerformIO)

import qualified HIE.Bios.Ghc.Gap as Gap
import HIE.Bios.Types
import HIE.Bios.Environment
import HIE.Bios.Flags



----------------------------------------------------------------

-- | Converting the 'Ghc' monad to the 'IO' monad.
withGHC :: FilePath  -- ^ A target file displayed in an error message.
        -> Ghc a -- ^ 'Ghc' actions created by the Ghc utilities.
        -> IO a
withGHC file body = ghandle ignore $ withGHC' body
  where
    ignore :: SomeException -> IO a
    ignore e = do
        hPutStr stderr $ file ++ ":0:0:Error:"
        hPrint stderr e
        exitSuccess

withGHC' :: Ghc a -> IO a
withGHC' body = do
    mlibdir <- getSystemLibDir
    G.runGhc mlibdir body

withGhcT :: (Exception.ExceptionMonad m, G.MonadIO m, Monad m) => GhcT m a -> m a
withGhcT body = do
  mlibdir <- G.liftIO $ getSystemLibDir
  G.runGhcT mlibdir body

----------------------------------------------------------------



initializeFlagsWithCradle ::
    GhcMonad m
    => FilePath -- The file we are loading it because of
    -> Cradle
    -> m (Maybe CradleError)
initializeFlagsWithCradle = initializeFlagsWithCradleWithMessage (Just G.batchMsg)

initializeFlagsWithCradleWithMessage ::
  GhcMonad m
  => Maybe G.Messager
  -> FilePath -- The file we are loading it because of
  -> Cradle
  -> m (Maybe CradleError)
initializeFlagsWithCradleWithMessage msg fp cradle = do
    compOpts <- liftIO $ getCompilerOptions fp cradle
    case compOpts of
      Left err -> return $ Just err
      Right opts -> initSessionWithMessage msg opts >> return Nothing

initSessionWithMessage :: (GhcMonad m)
            => Maybe G.Messager
            -> CompilerOptions
            -> m ()
initSessionWithMessage msg compOpts = do
    targets <- initSession compOpts
    G.setTargets targets
    -- Get the module graph using the function `getModuleGraph`
    mod_graph <- G.depanal [] True
    void $ G.load' LoadAllTargets msg mod_graph

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

withCmdFlags ::
  (GhcMonad m)
  => [String] -> m a ->  m a
withCmdFlags flags body = G.gbracket setup teardown (\_ -> body)
  where
    setup = do
        (dflag, _) <- G.getSessionDynFlags >>= addCmdOpts flags
        void $ G.setSessionDynFlags dflag
        return dflag
    teardown = void . G.setSessionDynFlags

----------------------------------------------------------------

setDeferTypeErrors :: DynFlags -> DynFlags
setDeferTypeErrors
    = foldDFlags (flip wopt_set) [Opt_WarnTypedHoles, Opt_WarnDeferredTypeErrors, Opt_WarnDeferredOutOfScopeVariables]
    . foldDFlags setGeneralFlag' [Opt_DeferTypedHoles, Opt_DeferTypeErrors, Opt_DeferOutOfScopeVariables]

foldDFlags :: (a -> DynFlags -> DynFlags) -> [a] -> DynFlags -> DynFlags
foldDFlags f xs x = foldr f x xs

-- | Set 'DynFlags' equivalent to "-w:".
setNoWarningFlags :: DynFlags -> DynFlags
setNoWarningFlags df = df { warningFlags = Gap.emptyWarnFlags}

-- | Set 'DynFlags' equivalent to "-Wall".
setAllWarningFlags :: DynFlags -> DynFlags
setAllWarningFlags df = df { warningFlags = allWarningFlags }


{-# NOINLINE allWarningFlags #-}
allWarningFlags :: Gap.WarnFlags
allWarningFlags = unsafePerformIO $ do
    mlibdir <- getSystemLibDir
    G.runGhcT mlibdir $ do
        df <- G.getSessionDynFlags
        (df', _) <- addCmdOpts ["-Wall"] df
        return $ G.warningFlags df'
