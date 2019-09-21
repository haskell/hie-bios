module HIE.Bios.Flags (getCompilerOptions, CradleError) where

import HIE.Bios.Types


-- | Initialize the 'DynFlags' relating to the compilation of a single
-- file or GHC session according to the 'Cradle' and 'Options'
-- provided.
getCompilerOptions ::
    FilePath -- The file we are loading it because of
    -> Cradle
    -> IO (CradleLoadResult CompilerOptions)
getCompilerOptions fp cradle = do
  getOptions (cradleOptsProg cradle) fp


----------------------------------------------------------------
