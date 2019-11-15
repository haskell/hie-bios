{- | The HIE Bios

Provides an abstraction over the GHC Api to initialise a GHC session and
loading modules in a project.

Defines the `hie.yaml` file specification. This is used to explicitly configure
how a project should be built by GHC.

-}
module HIE.Bios (
  -- * Find and load a Cradle
    Cradle(..)
  , CradleLoadResult(..)
  , CradleError(..)
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