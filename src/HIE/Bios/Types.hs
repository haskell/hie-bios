{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HIE.Bios.Types where

import System.Exit
import System.IO
import Control.Exception ( Exception )

data BIOSVerbosity = Silent | Verbose

data CradleOpts = CradleOpts
                { cradleOptsVerbosity :: BIOSVerbosity
                , cradleOptsHandle :: Maybe Handle
                -- ^ The handle where to send output to, if not set, stderr
                }

defaultCradleOpts :: CradleOpts
defaultCradleOpts = CradleOpts Silent Nothing

----------------------------------------------------------------

-- | The environment where this library is used.
data Cradle = Cradle {
  -- | The project root directory.
    cradleRootDir    :: FilePath
  -- | The action which needs to be executed to get the correct
  -- command line arguments
  , cradleOptsProg   :: CradleAction
  } deriving (Show)

data CradleAction = CradleAction {
                      actionName :: String
                      -- ^ Name of the action
                      , runCradle :: FilePath -> IO (CradleLoadResult ComponentOptions)
                      -- ^ Options to compile the given file with.
                      }

instance Show CradleAction where
  show CradleAction { actionName = name } = "CradleAction: " ++ name

data CradleLoadResult r = CradleSuccess r -- ^ The cradle succeeded and returned these options
                      | CradleFail CradleError -- ^ We tried to load the cradle and it failed
                      | CradleNone -- ^ No attempt was made to load the cradle
                      deriving (Functor)


data CradleError = CradleError ExitCode String deriving (Show)

instance Exception CradleError where
----------------------------------------------------------------

-- | Option information for GHC
data ComponentOptions = ComponentOptions {
    componentOptions  :: [String]  -- ^ Command line options
  , componentDependencies :: [FilePath]
  -- ^ Dependencies of a cradle that might change the cradle.
  -- Contains both files specified in hie.yaml as well as
  -- specified by the build-tool if there is any.
  -- FilePaths are expected to be relative to the `cradleRootDir`
  -- to which this CradleAction belongs to.
  -- Files returned by this action might not actually exist.
  -- This is useful, because, sometimes, adding specific files
  -- changes the options that a Cradle may return, thus, needs reload
  -- as soon as these files are created.
  } deriving (Eq, Show)