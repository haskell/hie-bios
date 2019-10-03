{-# LANGUAGE CPP #-}
module HIE.Bios.Log where

import Control.Monad.IO.Class
#if HS_LOGGER
import System.Log.Logger
#else
import System.IO (hPutStrLn, stderr)
#endif

logm :: MonadIO m => String -> m ()
#if HS_LOGGER
logm s = liftIO $ infoM "hie-bios" s
#else
logm s = liftIO $ hPutStrLn stderr ("hie-bios - LOG: " ++ s)
#endif

debugm :: MonadIO m => String -> m ()
#if HS_LOGGER
debugm s = liftIO $ debugM "hie-bios" s
#else
debugm s = liftIO $ hPutStrLn stderr ("hie-bios - DEBUG: " ++ s)
#endif

warningm :: MonadIO m => String -> m ()
#if HS_LOGGER
warningm s = liftIO $ warningM "hie-bios" s
#else
warningm s = liftIO $ hPutStrLn stderr ("hie-bios - WARNING: " ++ s)
#endif

errorm :: MonadIO m => String -> m ()
#if HS_LOGGER
errorm s = liftIO $ errorM "hie-bios" s
#else
errorm s = liftIO $ hPutStrLn stderr ("hie-bios - ERROR: " ++ s)
#endif
