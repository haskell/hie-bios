{-# LANGUAGE LambdaCase #-}
module HIE.Bios.Internal.Debug (debugInfo, rootInfo, configInfo, cradleInfo) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad

import qualified Data.Char as Char
import Data.Maybe (fromMaybe)

import HIE.Bios.Ghc.Api
import HIE.Bios.Cradle
import HIE.Bios.Types
import HIE.Bios.Flags

import System.Directory
import System.FilePath

----------------------------------------------------------------

-- | Obtain debug information for a 'Cradle'.
--
-- Tries to load the 'Cradle' and dump any information associated with it.
-- If loading succeeds, contains information such as the root directory of
-- the cradle, the compiler options to compile a module in this 'Cradle',
-- the file dependencies and so on.
--
-- Otherwise, shows the error message and exit-code.
debugInfo :: FilePath
          -> Cradle
          -> IO String
debugInfo fp cradle = unlines <$> do
    res <- getCompilerOptions fp cradle
    conf <- findConfig fp
    crdl <- findCradle' fp
    case res of
      CradleSuccess (ComponentOptions gopts deps) -> do
        mglibdir <- liftIO getSystemLibDir
        return [
            "Root directory:      " ++ rootDir
          , "GHC options:         " ++ unwords (map quoteIfNeeded gopts)
          , "System libraries:    " ++ fromMaybe "" mglibdir
          , "Config Location:     " ++ conf
          , "Cradle:              " ++ crdl
          , "Dependencies:        " ++ unwords deps
          ]
      CradleFail (CradleError ext stderr) ->
        return ["Cradle failed to load"
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
rootInfo :: Cradle
          -> IO String
rootInfo cradle = return $ cradleRootDir cradle

----------------------------------------------------------------

configInfo :: [FilePath] -> IO String
configInfo []   = return "No files given"
configInfo args = do
  cwd <- getCurrentDirectory
  fmap unlines $ forM args $ \fp ->
    (("Config for \"" ++ fp ++ "\": ") ++) <$> findConfig (cwd </> fp)

findConfig :: FilePath -> IO String
findConfig fp = findCradle fp >>= \case
  Just yaml -> return yaml
  _ -> return "No explicit config found"

----------------------------------------------------------------

cradleInfo :: [FilePath] -> IO String
cradleInfo [] = return "No files given"
cradleInfo args = do
  cwd <- getCurrentDirectory
  fmap unlines $ forM args $ \fp ->
    (("Cradle for \"" ++ fp ++ "\": ") ++)  <$> findCradle' (cwd </> fp)

findCradle' :: FilePath -> IO String
findCradle' fp = findCradle fp >>= \case
  Just yaml -> do
    crdl <- loadCradle yaml
    return $ show crdl
  Nothing -> do
    crdl <- loadImplicitCradle fp
    return $ show crdl