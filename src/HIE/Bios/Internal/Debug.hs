{-# LANGUAGE LambdaCase #-}
module HIE.Bios.Internal.Debug (debugInfo, rootInfo, configInfo, cradleInfo) where

import Control.Monad
import Data.Void
import Data.Foldable

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
          => FilePath
          -> Cradle a
          -> IO String
debugInfo fp cradle = unlines <$> do
    res <- getCompilerOptions fp cradle
    canonFp <- canonicalizePath fp
    conf <- findConfig canonFp
    crdl <- findCradle' canonFp
    ghcLibDir <- getRuntimeGhcLibDir cradle
    ghcVer <- getRuntimeGhcVersion cradle
    let printCradleData =
          [ "Root directory:        " ++ rootDir
          , "Cradle:                " ++ crdl
          , "GHC library directory: " ++ show ghcLibDir
          , "GHC version:           " ++ show ghcVer
          , "Config Location:       " ++ conf
          ]
    case res of
      CradleSuccess opts ->
        return $
          -- TODO: 'toList' might swallow main component as it might be Nothing
          printCradleData ++ (concatMap printComponentOptions (toList opts))
      CradleFail (CradleError deps ext stderr) ->
        return
          [ "Cradle failed to load"
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

    printComponentOptions (ComponentOptions gopts croot deps) =
      [ "Component directory:   " ++ croot
      , "GHC options:           " ++ unwords (map quoteIfNeeded gopts)
      , "Dependencies:          " ++ unwords deps
      ]

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

cradleInfo :: [FilePath] -> IO String
cradleInfo [] = return "No files given"
cradleInfo args =
  fmap unlines $ forM args $ \fp -> do
    fp' <- canonicalizePath fp
    (("Cradle for \"" ++ fp' ++ "\": ") ++)  <$> findCradle' fp'

findCradle' :: FilePath -> IO String
findCradle' fp =
  findCradle fp >>= \case
    Just yaml -> do
      crdl <- loadCradle yaml
      return $ show crdl
    Nothing -> do
      crdl <- loadImplicitCradle fp :: IO (Cradle Void)
      return $ show crdl
