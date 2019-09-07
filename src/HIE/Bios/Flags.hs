module HIE.Bios.Flags (getCompilerOptions, CradleError) where

import Control.Monad.IO.Class
import Control.Exception ( Exception )

import System.Exit (ExitCode(..))

import HIE.Bios.Types (Cradle, CompilerOptions(..), getOptions, cradleOptsProg)

-- | Initialize the 'DynFlags' relating to the compilation of a single
-- file or GHC session according to the 'Cradle' and 'Options'
-- provided.
getCompilerOptions ::
    FilePath -- The file we are loading it because of
    -> Cradle
    -> IO (Either CradleError CompilerOptions)
getCompilerOptions fp cradle = do
  (ex, err, ghcOpts) <- liftIO $ getOptions (cradleOptsProg cradle) fp
  case ex of
    ExitFailure _ -> return $ Left (CradleError err)
    _ -> do
        let compOpts = CompilerOptions ghcOpts
        return $ Right compOpts

data CradleError = CradleError String deriving (Show)

instance Exception CradleError where

----------------------------------------------------------------