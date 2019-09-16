{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module HIE.Bios.Cradle (
      findCradle
    , loadCradle
    , loadImplicitCradle
    , defaultCradle
  ) where

import System.Process
import System.Exit
import HIE.Bios.Types
import HIE.Bios.Config
import System.Directory hiding (findFile)
import Control.Monad.Trans.Maybe
import System.FilePath
import Control.Monad
import Control.Monad.IO.Class
import System.Info.Extra
import Control.Applicative ((<|>))
import Data.FileEmbed
import System.IO.Temp
import Data.List

import System.PosixCompat.Files

----------------------------------------------------------------

-- | Given root/foo/bar.hs, return root/hie.yaml, or wherever the yaml file was found
findCradle :: FilePath -> IO (Maybe FilePath)
findCradle wfile = do
    let wdir = takeDirectory wfile
    runMaybeT (yamlConfig wdir)

-- | Given root/hie.yaml load the Cradle
loadCradle :: FilePath -> IO Cradle
loadCradle = loadCradleWithOpts defaultCradleOpts

-- | Given root/foo/bar.hs, load an implicit cradle
loadImplicitCradle :: FilePath -> IO Cradle
loadImplicitCradle wfile = do
  let wdir = takeDirectory wfile
  cfg <- runMaybeT (implicitConfig wdir)
  return $ case cfg of
    Just bc -> getCradle bc
    Nothing -> defaultCradle wdir

-- | Finding 'Cradle'.
--   Find a cabal file by tracing ancestor directories.
--   Find a sandbox according to a cabal sandbox config
--   in a cabal directory.
loadCradleWithOpts :: CradleOpts -> FilePath -> IO Cradle
loadCradleWithOpts _copts wfile = do
    cradleConfig <- readCradleConfig wfile
    return $ getCradle (cradleConfig, takeDirectory wfile)

getCradle :: (CradleConfig, FilePath) -> Cradle
getCradle (cc, wdir) = case cc of
                 Cabal mc -> cabalCradle wdir mc
                 Stack -> stackCradle wdir
                 Bazel -> rulesHaskellCradle wdir
                 Obelisk -> obeliskCradle wdir
                 Bios bios -> biosCradle wdir bios
                 Direct xs -> directCradle wdir xs
                 Default   -> defaultCradle wdir

implicitConfig :: FilePath -> MaybeT IO (CradleConfig, FilePath)
implicitConfig fp =
         (\wdir -> (Bios (wdir </> ".hie-bios"), wdir)) <$> biosWorkDir fp
     <|> (Obelisk,) <$> obeliskWorkDir fp
     <|> (Bazel,) <$> rulesHaskellWorkDir fp
     <|> (Stack,) <$> stackWorkDir fp
     <|> ((Cabal Nothing,) <$> cabalWorkDir fp)


yamlConfig :: FilePath ->  MaybeT IO FilePath
yamlConfig fp = do
  configDir <- yamlConfigDirectory fp
  return (configDir </> configFileName)

yamlConfigDirectory :: FilePath -> MaybeT IO FilePath
yamlConfigDirectory = findFileUpwards (configFileName ==)

readCradleConfig :: FilePath -> IO CradleConfig
readCradleConfig yamlHie = do
  cfg  <- liftIO $ readConfig yamlHie
  return (cradle cfg)

configFileName :: FilePath
configFileName = "hie.yaml"


---------------------------------------------------------------
-- Default cradle has no special options, not very useful for loading
-- modules.

defaultCradle :: FilePath -> Cradle
defaultCradle cur_dir =
  Cradle {
      cradleRootDir = cur_dir
    , cradleOptsProg = CradleAction "default" (const $ return (ExitSuccess, "", []))
    }

-------------------------------------------------------------------------

directCradle :: FilePath -> [String] -> Cradle
directCradle wdir args =
  Cradle {
      cradleRootDir = wdir
    , cradleOptsProg = CradleAction "direct" (const $ return (ExitSuccess, "", args))
  }


-------------------------------------------------------------------------


-- | Find a cradle by finding an executable `hie-bios` file which will
-- be executed to find the correct GHC options to use.
biosCradle :: FilePath -> FilePath -> Cradle
biosCradle wdir bios =
  Cradle {
      cradleRootDir    = wdir
    , cradleOptsProg   = CradleAction "bios" (biosAction wdir bios)
  }

biosWorkDir :: FilePath -> MaybeT IO FilePath
biosWorkDir = findFileUpwards (".hie-bios" ==)


biosAction :: FilePath -> FilePath -> FilePath -> IO (ExitCode, String, [String])
biosAction _wdir bios fp = do
  bios' <- canonicalizePath bios
  (ex, res, std) <- readProcessWithExitCode bios' [fp] []
  return (ex, std, words res)

------------------------------------------------------------------------
-- Cabal Cradle
-- Works for new-build by invoking `v2-repl` does not support components
-- yet.

cabalCradle :: FilePath -> Maybe String -> Cradle
cabalCradle wdir mc =
  Cradle {
      cradleRootDir    = wdir
    , cradleOptsProg   = CradleAction "cabal" (cabalAction wdir mc)
  }

cabalWrapper :: String
cabalWrapper = $(embedStringFile "wrappers/cabal")

cabalWrapperHs :: String
cabalWrapperHs = $(embedStringFile "wrappers/cabal.hs")

processCabalWrapperArgs :: String -> Maybe [String]
processCabalWrapperArgs args =
    case lines args of
        [dir, ghc_args] ->
            let final_args =
                    removeInteractive
                    $ map (fixImportDirs dir)
                    $ limited ghc_args
            in Just final_args
        _ -> Nothing
  where
    limited :: String -> [String]
    limited = unfoldr $ \argstr ->
        if null argstr
        then Nothing
        else
            let (arg, argstr') = break (== '\NUL') argstr
            in Just (arg, drop 1 argstr')

-- generate a fake GHC that can be passed to cabal
-- when run with --interactive, it will print out its
-- command-line arguments and exit
getCabalWrapperTool :: IO FilePath
getCabalWrapperTool = do
  wrapper_fp <-
    if isWindows
      then do
        wrapper_hs <- writeSystemTempFile "wrapper.hs" cabalWrapperHs
        -- the initial contents will be overwritten immediately after by ghc
        wrapper_fp <- writeSystemTempFile "wrapper.exe" ""
        let ghc = (proc "ghc" ["-o", wrapper_fp, wrapper_hs])
                    { cwd = Just (takeDirectory wrapper_hs) }
        readCreateProcess ghc "" >>= putStr
        return wrapper_fp
      else do
        writeSystemTempFile "bios-wrapper" cabalWrapper
  setFileMode wrapper_fp accessModes
  _check <- readFile wrapper_fp
  return wrapper_fp

cabalAction :: FilePath -> Maybe String -> FilePath -> IO (ExitCode, String, [String])
cabalAction work_dir mc _fp = do
  wrapper_fp <- getCabalWrapperTool
  let cab_args = ["v2-repl", "-v0", "--disable-documentation", "--with-compiler", wrapper_fp]
                  ++ [component_name | Just component_name <- [mc]]
  (ex, args, stde) <-
    readProcessWithExitCodeInDirectory work_dir "cabal" cab_args []
  case processCabalWrapperArgs args of
      Nothing -> error (show (ex, stde, args))
      Just final_args -> pure (ex, stde, final_args)

removeInteractive :: [String] -> [String]
removeInteractive = filter (/= "--interactive")

fixImportDirs :: FilePath -> String -> String
fixImportDirs base_dir arg =
  if "-i" `isPrefixOf` arg
    then let dir = drop 2 arg
         in if isRelative dir then ("-i" ++ base_dir ++ "/" ++ dir)
                              else arg
    else arg


cabalWorkDir :: FilePath -> MaybeT IO FilePath
cabalWorkDir = findFileUpwards isCabal
  where
    isCabal name = name == "cabal.project"

------------------------------------------------------------------------
-- Stack Cradle
-- Works for by invoking `stack repl` with a wrapper script

stackCradle :: FilePath -> Cradle
stackCradle wdir =
  Cradle {
      cradleRootDir    = wdir
    , cradleOptsProg   = CradleAction "stack" (stackAction wdir)
  }

-- Same wrapper works as with cabal
stackWrapper :: String
stackWrapper = $(embedStringFile "wrappers/cabal")

stackAction :: FilePath -> FilePath -> IO (ExitCode, String, [String])
stackAction work_dir fp = do
  wrapper_fp <- writeSystemTempFile "wrapper" stackWrapper
  -- TODO: This isn't portable for windows
  setFileMode wrapper_fp accessModes
  -- TODO: this is for debugging
  -- check <- readFile wrapper_fp
  -- traceM check
  (ex1, args, stde) <-
      readProcessWithExitCodeInDirectory work_dir "stack" ["repl", "--silent", "--no-load", "--with-ghc", wrapper_fp, fp ] []
  (ex2, pkg_args, stdr) <-
      readProcessWithExitCodeInDirectory work_dir "stack" ["path", "--ghc-package-path"] []
  let split_pkgs = splitSearchPath (init pkg_args)
      pkg_ghc_args = concatMap (\p -> ["-package-db", p] ) split_pkgs
  case processCabalWrapperArgs args of
      Nothing -> error (show (ex1, stde, args))
      Just ghc_args -> return (combineExitCodes [ex1, ex2], stde ++ stdr, ghc_args ++ pkg_ghc_args)

combineExitCodes :: [ExitCode] -> ExitCode
combineExitCodes = foldr go ExitSuccess
  where
    go ExitSuccess b = b
    go a _ = a



stackWorkDir :: FilePath -> MaybeT IO FilePath
stackWorkDir = findFileUpwards isStack
  where
    isStack name = name == "stack.yaml"


----------------------------------------------------------------------------
-- rules_haskell - Thanks for David Smith for helping with this one.
-- Looks for the directory containing a WORKSPACE file
--
rulesHaskellWorkDir :: FilePath -> MaybeT IO FilePath
rulesHaskellWorkDir fp =
  findFileUpwards (== "WORKSPACE") fp

rulesHaskellCradle :: FilePath -> Cradle
rulesHaskellCradle wdir =
  Cradle {
      cradleRootDir  = wdir
    , cradleOptsProg   = CradleAction "bazel" (rulesHaskellAction wdir)
    }


bazelCommand :: String
bazelCommand = $(embedStringFile "wrappers/bazel")

rulesHaskellAction :: FilePath -> FilePath -> IO (ExitCode, String, [String])
rulesHaskellAction work_dir fp = do
  wrapper_fp <- writeSystemTempFile "wrapper" bazelCommand
  -- TODO: This isn't portable for windows
  setFileMode wrapper_fp accessModes
  let rel_path = makeRelative work_dir fp
  (ex, args, stde) <-
      readProcessWithExitCodeInDirectory work_dir wrapper_fp [rel_path] []
  let args'  = filter (/= '\'') args
  let args'' = filter (/= "\"$GHCI_LOCATION\"") (words args')
  return (ex, stde, args'')


------------------------------------------------------------------------------
-- Obelisk Cradle
-- Searches for the directory which contains `.obelisk`.

obeliskWorkDir :: FilePath -> MaybeT IO FilePath
obeliskWorkDir fp = do
  -- Find a possible root which will contain the cabal.project
  wdir <- findFileUpwards (== "cabal.project") fp
  -- Check for the ".obelisk" folder in this directory
  check <- liftIO $ doesDirectoryExist (wdir </> ".obelisk")
  unless check (fail "Not obelisk dir")
  return wdir


obeliskCradle :: FilePath -> Cradle
obeliskCradle wdir =
  Cradle {
      cradleRootDir  = wdir
    , cradleOptsProg = CradleAction "obelisk" (obeliskAction wdir)
    }

obeliskAction :: FilePath -> FilePath -> IO (ExitCode, String, [String])
obeliskAction work_dir _fp = do
  (ex, args, stde) <-
      readProcessWithExitCodeInDirectory work_dir "ob" ["ide-args"] []
  return (ex, stde, words args)


------------------------------------------------------------------------------
-- Utilities


-- | Searches upwards for the first directory containing a file to match
-- the predicate.
findFileUpwards :: (FilePath -> Bool) -> FilePath -> MaybeT IO FilePath
findFileUpwards p dir = do
    cnts <- liftIO $ findFile p dir
    case cnts of
        [] | dir' == dir -> fail "No cabal files"
           | otherwise   -> findFileUpwards p dir'
        _:_          -> return dir
  where
    dir' = takeDirectory dir

-- | Sees if any file in the directory matches the predicate
findFile :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
findFile p dir = getFiles >>= filterM doesPredFileExist
  where
    getFiles = filter p <$> getDirectoryContents dir
    doesPredFileExist file = doesFileExist $ dir </> file

-- | Call a process with the given arguments and the given stdin
-- in the given working directory.
readProcessWithExitCodeInDirectory
  :: FilePath -> FilePath -> [String] -> String -> IO (ExitCode, String, String)
readProcessWithExitCodeInDirectory work_dir fp args stdin =
  let process = (proc fp args) { cwd = Just work_dir }
  in  readCreateProcessWithExitCode process stdin
