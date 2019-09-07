-- | The HIE Bios

module HIE.Bios (
  -- * Initialise a session
    Cradle(..)
  , findCradle
  , defaultCradle
  -- * Compiler Options
  , CompilerOptions(..)
  , getCompilerOptions
  -- * Init session
  , initSession
  ) where

import HIE.Bios.Cradle
import HIE.Bios.Types
import HIE.Bios.Flags
import HIE.Bios.Environment
