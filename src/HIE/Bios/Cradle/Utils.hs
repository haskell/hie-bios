module HIE.Bios.Cradle.Utils
  (
  -- * Helper for process errors
    ProcessErrorDetails(..)
  , prettyProcessErrorDetails
  -- * Cradle utils
  , selectCradle
  -- * Processing of ghc-options
  , removeInteractive
  , removeRTS
  , removeVerbosityOpts
  , expandGhcOptionResponseFile
  )
  where

import HIE.Bios.Types (prettyCmdSpec)

import Data.List
import System.Process.Extra
import GHC.ResponseFile (expandResponse)
import HIE.Bios.Ghc.Gap (removeRTS)

-- ----------------------------------------------------------------------------
-- Process error details
-- ----------------------------------------------------------------------------

data ProcessErrorDetails = ProcessErrorDetails
  { processCmd :: CmdSpec
  -- ^ The 'CmdSpec' of the command.
  , processStdout :: [String]
  -- ^ The stdout of the command.
  , processStderr :: [String]
  -- ^ The stderr of the command.
  , processGhcOptions :: [String]
  -- ^ The ghc-options that were obtained via the command
  , processHieBiosEnvironment :: [(String, String)]
  -- ^ Environment variables populated by 'hie-bios' and their respective value.
  }

prettyProcessErrorDetails :: ProcessErrorDetails -> [String]
prettyProcessErrorDetails p  =
  [ "Failed command: " <> prettyCmdSpec (processCmd p),
    unlines (processStdout p),
    unlines (processStderr p),
    unlines (processGhcOptions p),
    "Process Environment:"
  ] <> [ key <> ": " <> value
  | (key, value) <- processHieBiosEnvironment p
  ]

-- ----------------------------------------------------------------------------
-- Cradle utils
-- ----------------------------------------------------------------------------

-- | Given a list of cradles, try to find the most likely cradle that
-- this 'FilePath' belongs to.
-- TODO: this finds the first cradle that matches, while hie-bios README says most-specific cradle would match, unless the inputs are assumed sorted by specificity?
selectCradle :: (a -> FilePath) -> FilePath -> [a] -> Maybe a
selectCradle _ _ [] = Nothing
selectCradle k cur_fp (c: css) =
    if k c `isPrefixOf` cur_fp
      then Just c
      else selectCradle k cur_fp css


-- ----------------------------------------------------------------------------
-- Cradle utils
-- ----------------------------------------------------------------------------

removeInteractive :: [String] -> [String]
removeInteractive = filter (/= "--interactive")

removeVerbosityOpts :: [String] -> [String]
removeVerbosityOpts = filter ((&&) <$> (/= "-v0") <*> (/= "-w"))

expandGhcOptionResponseFile :: [String] -> IO [String]
expandGhcOptionResponseFile args = do
  expanded_args <- expandResponse args
  pure $ removeInteractive expanded_args
