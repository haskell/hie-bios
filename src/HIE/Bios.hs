-- | The HIE Bios

module HIE.Bios (
  -- * Find and load a Cradle
    Cradle(..)
  , CradleLoadResult(..)
  , findCradle
  , loadCradle
  , loadImplicitCradle
  , defaultCradle
  -- * Compiler Options
  , ComponentOptions(..)
  , getCompilerOptions
  -- * Initialising a GHC session from a Cradle
  , initSession
  -- * Loading targets into a GHC session
  , loadFile

  ) where

import HIE.Bios.Cradle
import HIE.Bios.Types
import HIE.Bios.Flags
import HIE.Bios.Environment
import HIE.Bios.Ghc.Load