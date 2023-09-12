module HIE.Bios.Flags (getCompilerOptions) where

import HIE.Bios.Types

import Colog.Core (WithSeverity (..), Severity (..), (<&))

-- | Initialize the 'DynFlags' relating to the compilation of a single
-- file or GHC session according to the provided 'Cradle'.
getCompilerOptions
  :: FilePath -- ^ The file we are loading it because of
  -> [FilePath] -- ^ previous files we might want to include in the build
  -> Cradle a
  -> IO (CradleLoadResult ComponentOptions)
getCompilerOptions fp fps cradle = do
  (cradleLogger cradle) <& LogProcessOutput "invoking build tool to determine build flags (this may take some time depending on the cache)" `WithSeverity` Info
  runCradle (cradleOptsProg cradle) fp fps
