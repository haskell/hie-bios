module HIE.Bios.Flags (getCompilerOptions) where

import HIE.Bios.Types

import Colog.Core (WithSeverity (..), Severity (..), (<&))

-- | Initialize the 'DynFlags' relating to the compilation of a single
-- file or GHC session according to the provided 'Cradle'.

-- | Note: @getCompilerOptions (TargetWithContext path cxt) LoadFile@ will warn if `cxt` is non-empty.
getCompilerOptions :: TargetWithContext -> LoadMode -> Cradle a -> IO (CradleLoadResult ComponentOptions)
getCompilerOptions target loadMode cradle = do
  (cradleLogger cradle) <& LogProcessOutput "invoking build tool to determine build flags (this may take some time depending on the cache)" `WithSeverity` Info
  runCradle (cradleOptsProg cradle) target loadMode
