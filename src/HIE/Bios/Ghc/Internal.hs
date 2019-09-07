-- | The Happy Haskell Programming library in low level.

module HIE.Bios.Ghc.Internal (
  -- * Types
  CompilerOptions(..)
  -- * IO
  , getDynamicFlags
  -- * Targets
  , setTargetFiles
  -- * Logging
  , withLogger
  , setNoWaringFlags
  , setAllWaringFlags
  ) where

import HIE.Bios.Ghc.Api
import HIE.Bios.Ghc.Logger
import HIE.Bios.Types
