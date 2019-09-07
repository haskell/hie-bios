module HIE.Bios.Flags where


-- | Initialize the 'DynFlags' relating to the compilation of a single
-- file or GHC session according to the 'Cradle' and 'Options'
-- provided.
getCompilerOptions ::
    FilePath -- The file we are loading it because of
    -> Cradle
    -> IO CompilerOptions
getCompilerOptions fp cradle = do
  (ex, err, ghcOpts) <- liftIO $ getOptions (cradleOptsProg cradle) fp
  G.pprTrace "res" (G.text (show (ex, err, ghcOpts, fp))) (return ())
  case ex of
    ExitFailure _ -> throwCradleError err
    _ -> return ()
  let compOpts = CompilerOptions ghcOpts
  liftIO $ hPrint stderr ghcOpts
  return compOpts

data CradleError = CradleError String deriving (Show)

instance Exception CradleError where

throwCradleError :: GhcMonad m => String -> m ()
throwCradleError = liftIO . throwIO . CradleError

----------------------------------------------------------------