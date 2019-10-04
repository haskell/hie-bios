module HIE.Bios.Log where

import Control.Monad.IO.Class
import System.Log.Logger

logm :: MonadIO m => String -> m ()
logm s = liftIO $ infoM "hie-bios" s

debugm :: MonadIO m => String -> m ()
debugm s = liftIO $ debugM "hie-bios" s

warningm :: MonadIO m => String -> m ()
warningm s = liftIO $ warningM "hie-bios" s

errorm :: MonadIO m => String -> m ()
errorm s = liftIO $ errorM "hie-bios" s
