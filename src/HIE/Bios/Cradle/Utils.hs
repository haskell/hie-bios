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
  -- * Processing of process output
  , trim
  )
  where

import HIE.Bios.Types (prettyCmdSpec)

import Data.Char (isSpace)
import Data.List
import System.Process.Extra
import GHC.ResponseFile (expandResponse)

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

-- | Strip out any ["+RTS", ..., "-RTS"] sequences in the command string list.
data InRTS = OutsideRTS | InsideRTS

-- | Strip out any ["+RTS", ..., "-RTS"] sequences in the command string list.
--
-- >>> removeRTS ["option1", "+RTS -H32m -RTS", "option2"]
-- ["option1", "option2"]
--
-- >>> removeRTS ["option1", "+RTS", "-H32m", "-RTS", "option2"]
-- ["option1", "option2"]
--
-- >>> removeRTS ["option1", "+RTS -H32m"]
-- ["option1"]
--
-- >>> removeRTS ["option1", "+RTS -H32m", "-RTS", "option2"]
-- ["option1", "option2"]
--
-- >>> removeRTS ["option1", "+RTS -H32m", "-H32m -RTS", "option2"]
-- ["option1", "option2"]
removeRTS :: [String] -> [String]
removeRTS = go OutsideRTS
  where
    go :: InRTS -> [String] -> [String]
    go _ [] = []
    go OutsideRTS (y:ys)
      | "+RTS" `isPrefixOf` y = go (if "-RTS" `isSuffixOf` y then OutsideRTS else InsideRTS) ys
      | otherwise = y : go OutsideRTS ys
    go InsideRTS (y:ys) = go (if "-RTS" `isSuffixOf` y then OutsideRTS else InsideRTS) ys


removeVerbosityOpts :: [String] -> [String]
removeVerbosityOpts = filter ((&&) <$> (/= "-v0") <*> (/= "-w"))

expandGhcOptionResponseFile :: [String] -> IO [String]
expandGhcOptionResponseFile args = do
  expanded_args <- expandResponse args
  pure $ removeInteractive expanded_args

-- | Take the last line of output and strip trailing whitespace.
-- This is necessary because some tools (e.g. darcs, 7zip via stack)
-- produce extra output lines before the version number.
trim :: String -> String
trim s = case lines s of
  [] -> s
  ls -> dropWhileEnd isSpace $ last ls

