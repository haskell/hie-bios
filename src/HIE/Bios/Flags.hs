module HIE.Bios.Flags (getCompilerOptions) where

import HIE.Bios.Types

import qualified Colog.Core as L
import Data.Text.Prettyprint.Doc

-- | Initialize the 'DynFlags' relating to the compilation of a single
-- file or GHC session according to the provided 'Cradle'.
getCompilerOptions
  :: L.LogAction IO (L.WithSeverity Log)
  -> FilePath -- The file we are loading it because of
  -> Cradle a
  -> IO (CradleLoadResult ComponentOptions)
getCompilerOptions l fp cradle = runCradle (cradleOptsProg cradle) l fp
