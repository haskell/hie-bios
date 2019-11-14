module HIE.Bios.Flags (getCompilerOptions) where

import HIE.Bios.Types


-- | Initialize the 'DynFlags' relating to the compilation of a single
-- file or GHC session according to the provided 'Cradle'.
getCompilerOptions ::
    FilePath -- The file we are loading it because of
    -> Cradle
    -> IO (CradleLoadResult ComponentOptions)
getCompilerOptions fp cradle =
  runCradle (cradleOptsProg cradle) fp


----------------------------------------------------------------
