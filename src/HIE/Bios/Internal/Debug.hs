{-# LANGUAGE LambdaCase #-}
module HIE.Bios.Internal.Debug (debugInfo, rootInfo, configInfo, cradleInfo) where

import Control.Monad
import Colog.Core (LogAction (..), WithSeverity (..))
import Data.Void

import qualified Data.Char as Char

import HIE.Bios.Cradle
import HIE.Bios.Environment
import HIE.Bios.Types
import HIE.Bios.Flags

import System.Directory

----------------------------------------------------------------

-- | Obtain debug information for a 'Cradle'.
--
-- Tries to load the 'Cradle' and dump any information associated with it.
-- If loading succeeds, contains information such as the root directory of
-- the cradle, the compiler options to compile a module in this 'Cradle',
-- the file dependencies and so on.
--
-- Otherwise, shows the error message and exit-code.
debugInfo :: Show a
          => LogAction IO (WithSeverity Log)
          -> FilePath
          -> Cradle a
          -> IO String
debugInfo logger fp cradle = unlines <$> do
    res <- getCompilerOptions logger fp cradle
    canonFp <- canonicalizePath fp
    conf <- findConfig canonFp
    crdl <- findCradle' logger canonFp
    ghcLibDir <- getRuntimeGhcLibDir cradle
    ghcVer <- getRuntimeGhcVersion cradle
    case res of
      CradleSuccess (ComponentOptions gopts croot deps) -> do
        return [
            "Root directory:        " ++ rootDir
          , "Component directory:   " ++ croot
          , "GHC options:           " ++ unwords (map quoteIfNeeded gopts)
          , "GHC library directory: " ++ show ghcLibDir
          , "GHC version:           " ++ show ghcVer
          , "Config Location:       " ++ conf
          , "Cradle:                " ++ crdl
          , "Dependencies:          " ++ unwords deps
          ]
      CradleFail (CradleError deps ext stderr) ->
        return ["Cradle failed to load"
               , "Deps: " ++ show deps
               , "Exit Code: " ++ show ext
               , "Stderr: " ++ unlines stderr]
      CradleNone ->
        return ["No cradle"]
  where
    rootDir    = cradleRootDir cradle
    quoteIfNeeded option
      | any Char.isSpace option = "\"" ++ option ++ "\""
      | otherwise = option

----------------------------------------------------------------

-- | Get the root directory of the given Cradle.
rootInfo :: Cradle a
          -> IO String
rootInfo cradle = return $ cradleRootDir cradle

----------------------------------------------------------------

configInfo :: [FilePath] -> IO String
configInfo []   = return "No files given"
configInfo args =
  fmap unlines $ forM args $ \fp -> do
    fp' <- canonicalizePath fp
    (("Config for \"" ++ fp' ++ "\": ") ++) <$> findConfig fp'

findConfig :: FilePath -> IO String
findConfig fp = findCradle fp >>= \case
  Just yaml -> return yaml
  _ -> return "No explicit config found"

----------------------------------------------------------------

cradleInfo :: LogAction IO (WithSeverity Log) -> [FilePath] -> IO String
cradleInfo _ [] = return "No files given"
cradleInfo l args =
  fmap unlines $ forM args $ \fp -> do
    fp' <- canonicalizePath fp
    (("Cradle for \"" ++ fp' ++ "\": ") ++)  <$> findCradle' l fp'

findCradle' :: LogAction IO (WithSeverity Log) -> FilePath -> IO String
findCradle' l fp =
  findCradle fp >>= \case
    Just yaml -> do
      crdl <- loadCradle l yaml
      return $ show crdl
    Nothing -> do
      crdl <- loadImplicitCradle l fp :: IO (Cradle Void)
      return $ show crdl
