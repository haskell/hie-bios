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
import Data.Ord (Down(..))

import System.PosixCompat.Files

import Data.Version (showVersion)
import Paths_hie_bios
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
getCradle (cc, wdir) = addCradleDeps cradleDeps $ case cradleType cc of
    Cabal mc -> cabalCradle wdir mc
    CabalMulti ms ->
      getCradle $
        (CradleConfig cradleDeps
          (Multi [(p, CradleConfig [] (Cabal (Just c))) | (p, c) <- ms])
        , wdir)
    Stack -> stackCradle wdir
    Bazel -> rulesHaskellCradle wdir
    Obelisk -> obeliskCradle wdir
    Bios bios deps  -> biosCradle wdir bios deps
    Direct xs -> directCradle wdir xs
    Default   -> defaultCradle wdir
    None      -> noneCradle wdir
    Multi ms  -> multiCradle wdir ms
    where
      cradleDeps = cradleDependencies cc

addCradleDeps :: [FilePath] -> Cradle -> Cradle
addCradleDeps deps c =
  c { cradleOptsProg = addActionDeps (cradleOptsProg c) }
  where
    addActionDeps :: CradleAction -> CradleAction
    addActionDeps ca =
      ca { runCradle = \fp ->
            (fmap (\(ComponentOptions os' ds) -> ComponentOptions os' (ds `union` deps)))
              <$> runCradle ca fp }

implicitConfig :: FilePath -> MaybeT IO (CradleConfig, FilePath)
implicitConfig fp = do
  (crdType, wdir) <- implicitConfig' fp
  return (CradleConfig [] crdType, wdir)

implicitConfig' :: FilePath -> MaybeT IO (CradleType, FilePath)
implicitConfig' fp = (\wdir ->
         (Bios (wdir </> ".hie-bios") Nothing, wdir)) <$> biosWorkDir fp
     <|> (Obelisk,) <$> obeliskWorkDir fp
     <|> (Bazel,) <$> rulesHaskellWorkDir fp
     <|> (stackExecutable >> (Stack,) <$> stackWorkDir fp)
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
  Cradle
    { cradleRootDir = cur_dir
    , cradleOptsProg = CradleAction
        { actionName = "default"
        , runCradle = const $ return (CradleSuccess (ComponentOptions [] []))
        }
    }

---------------------------------------------------------------
-- The none cradle tells us not to even attempt to load a certain directory

noneCradle :: FilePath -> Cradle
noneCradle cur_dir =
  Cradle
    { cradleRootDir = cur_dir
    , cradleOptsProg = CradleAction
        { actionName = "none"
        , runCradle = const $ return CradleNone
        }
    }

---------------------------------------------------------------
-- The multi cradle selects a cradle based on the filepath

multiCradle :: FilePath -> [(FilePath, CradleConfig)] -> Cradle
multiCradle cur_dir cs =
  Cradle
    { cradleRootDir = cur_dir
    , cradleOptsProg = CradleAction
        { actionName = "multi"
        , runCradle = canonicalizePath >=> multiAction cur_dir cs
        }
    }

multiAction :: FilePath
            -> [(FilePath, CradleConfig)]
            -> FilePath
            -> IO (CradleLoadResult ComponentOptions)
multiAction cur_dir cs cur_fp = selectCradle =<< canonicalizeCradles

  where
    err_msg = unlines $ ["Multi Cradle: No prefixes matched"
                      , "pwd: " ++ cur_dir
                      , "filepath" ++ cur_fp
                      , "prefixes:"
                      ] ++ [show (pf, cradleType cc) | (pf, cc) <- cs]

    -- Canonicalize the relative paths present in the multi-cradle and
    -- also order the paths by most specific first. In the cradle selection
    -- function we want to choose the most specific cradle possible.
    canonicalizeCradles :: IO [(FilePath, CradleConfig)]
    canonicalizeCradles =
      sortOn (Down . fst)
        <$> mapM (\(p, c) -> (,c) <$> (canonicalizePath (cur_dir </> p))) cs

    selectCradle [] =
      return (CradleFail (CradleError ExitSuccess err_msg))
    selectCradle ((p, c): css) = do
        if p `isPrefixOf` cur_fp
          then runCradle (cradleOptsProg (getCradle (c, cur_dir))) cur_fp
          else selectCradle css


-------------------------------------------------------------------------

directCradle :: FilePath -> [String] -> Cradle
directCradle wdir args  =
  Cradle
    { cradleRootDir = wdir
    , cradleOptsProg = CradleAction
        { actionName = "direct"
        , runCradle = const $ return (CradleSuccess (ComponentOptions args []))
        }
    }

-------------------------------------------------------------------------


-- | Find a cradle by finding an executable `hie-bios` file which will
-- be executed to find the correct GHC options to use.
biosCradle :: FilePath -> FilePath -> Maybe FilePath -> Cradle
biosCradle wdir biosProg biosDepsProg =
  Cradle
    { cradleRootDir    = wdir
    , cradleOptsProg   = CradleAction
        { actionName = "bios"
        , runCradle = biosAction wdir biosProg biosDepsProg
        }
    }

biosWorkDir :: FilePath -> MaybeT IO FilePath
biosWorkDir = findFileUpwards (".hie-bios" ==)

biosDepsAction :: Maybe FilePath -> IO [FilePath]
biosDepsAction (Just biosDepsProg) = do
  biosDeps' <- canonicalizePath biosDepsProg
  (ex, sout, serr) <- readProcessWithExitCode biosDeps' [] []
  case ex of
    ExitFailure _ ->  error $ show (ex, sout, serr)
    ExitSuccess -> return (lines sout)
biosDepsAction Nothing = return []

biosAction :: FilePath
           -> FilePath
           -> Maybe FilePath
           -> FilePath
           -> IO (CradleLoadResult ComponentOptions)
biosAction _wdir bios bios_deps fp = do
  bios' <- canonicalizePath bios
  (ex, res, std) <- readProcessWithExitCode bios' [fp] []
  deps <- biosDepsAction bios_deps
        -- Output from the program should be delimited by newlines.
        -- Execute the bios action and add dependencies of the cradle.
        -- Removes all duplicates.
  return $ makeCradleResult (ex, std, lines res) deps

------------------------------------------------------------------------
-- Cabal Cradle
-- Works for new-build by invoking `v2-repl` does not support components
-- yet.

cabalCradle :: FilePath -> Maybe String -> Cradle
cabalCradle wdir mc =
  Cradle
    { cradleRootDir    = wdir
    , cradleOptsProg   = CradleAction
        { actionName = "cabal"
        , runCradle = cabalAction wdir mc
        }
    }

cabalCradleDependencies :: FilePath -> IO [FilePath]
cabalCradleDependencies rootDir = do
    cabalFiles <- findCabalFiles rootDir
    return $ cabalFiles ++ ["cabal.project"]

findCabalFiles :: FilePath -> IO [FilePath]
findCabalFiles wdir = do
  dirContent <- listDirectory wdir
  return $ filter ((== ".cabal") . takeExtension) dirContent

cabalWrapper :: String
cabalWrapper = $(embedStringFile "wrappers/cabal")

cabalWrapperHs :: String
cabalWrapperHs = $(embedStringFile "wrappers/cabal.hs")

processCabalWrapperArgs :: String -> Maybe [String]
processCabalWrapperArgs args =
    case lines args of
        [dir, ghc_args] ->
            let final_args =
                    removeVerbosityOpts
                    $ removeInteractive
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
        cacheDir <- getXdgDirectory XdgCache "hie-bios"
        let wrapper_name = "wrapper-" ++ showVersion version
        let wrapper_fp = cacheDir </> wrapper_name <.> "exe"
        exists <- doesFileExist wrapper_fp
        unless exists $ do
            tempDir <- getTemporaryDirectory
            let wrapper_hs = tempDir </> wrapper_name <.> "hs"
            writeFile wrapper_hs cabalWrapperHs
            createDirectoryIfMissing True cacheDir
            let ghc = (proc "ghc" ["-o", wrapper_fp, wrapper_hs])
                        { cwd = Just (takeDirectory wrapper_hs) }
            readCreateProcess ghc "" >>= putStr
        return wrapper_fp
      else do
        writeSystemTempFile "bios-wrapper" cabalWrapper

  setFileMode wrapper_fp accessModes
  _check <- readFile wrapper_fp
  return wrapper_fp

cabalAction :: FilePath -> Maybe String -> FilePath -> IO (CradleLoadResult ComponentOptions)
cabalAction work_dir mc _fp = do
  wrapper_fp <- getCabalWrapperTool
  let cab_args = ["v2-repl", "-v0", "--disable-documentation", "--with-compiler", wrapper_fp]
                  ++ [component_name | Just component_name <- [mc]]
  (ex, args, stde) <-
    readProcessWithExitCodeInDirectory work_dir "cabal" cab_args []

  deps <- cabalCradleDependencies work_dir
  case processCabalWrapperArgs args of
      Nothing -> pure $ CradleFail (CradleError ex
                  (unlines ["Failed to parse result of calling cabal"
                           , stde
                           , args]))
      Just final_args -> pure $ makeCradleResult (ex, stde, final_args) deps

removeInteractive :: [String] -> [String]
removeInteractive = filter (/= "--interactive")

removeVerbosityOpts :: [String] -> [String]
removeVerbosityOpts = filter ((&&) <$> (/= "-v0") <*> (/= "-w"))

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
  Cradle
    { cradleRootDir    = wdir
    , cradleOptsProg   = CradleAction
        { actionName = "stack"
        , runCradle = stackAction wdir
        }
    }

stackCradleDependencies :: FilePath -> IO [FilePath]
stackCradleDependencies wdir = do
    cabalFiles <- findCabalFiles wdir
    return $ cabalFiles ++ ["package.yaml", "stack.yaml"]

stackAction :: FilePath -> FilePath -> IO (CradleLoadResult ComponentOptions)
stackAction work_dir fp = do
  -- Same wrapper works as with cabal
  wrapper_fp <- getCabalWrapperTool
  (ex1, args, stde) <-
      readProcessWithExitCodeInDirectory work_dir "stack" ["repl", "--silent", "--no-load", "--with-ghc", wrapper_fp, fp ] []
  (ex2, pkg_args, stdr) <-
      readProcessWithExitCodeInDirectory work_dir "stack" ["path", "--ghc-package-path"] []
  let split_pkgs = splitSearchPath (init pkg_args)
      pkg_ghc_args = concatMap (\p -> ["-package-db", p] ) split_pkgs
  deps <- stackCradleDependencies work_dir
  return $ case processCabalWrapperArgs args of
      Nothing -> CradleFail (CradleError ex1
                  (unlines ["Failed to parse result of calling cabal"
                           , stde
                           , args]))

      Just ghc_args -> makeCradleResult (combineExitCodes [ex1, ex2], stde ++ stdr, ghc_args ++ pkg_ghc_args) deps

combineExitCodes :: [ExitCode] -> ExitCode
combineExitCodes = foldr go ExitSuccess
  where
    go ExitSuccess b = b
    go a _ = a

stackExecutable :: MaybeT IO FilePath
stackExecutable = MaybeT $ findExecutable "stack"

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
  Cradle
    { cradleRootDir  = wdir
    , cradleOptsProg   = CradleAction
        { actionName = "bazel"
        , runCradle = rulesHaskellAction wdir
        }
    }

rulesHaskellCradleDependencies :: FilePath -> IO [FilePath]
rulesHaskellCradleDependencies _wdir = return ["BUILD.bazel", "WORKSPACE"]

bazelCommand :: String
bazelCommand = $(embedStringFile "wrappers/bazel")

rulesHaskellAction :: FilePath -> FilePath -> IO (CradleLoadResult ComponentOptions)
rulesHaskellAction work_dir fp = do
  wrapper_fp <- writeSystemTempFile "wrapper" bazelCommand
  setFileMode wrapper_fp accessModes
  let rel_path = makeRelative work_dir fp
  (ex, args, stde) <-
      readProcessWithExitCodeInDirectory work_dir wrapper_fp [rel_path] []
  let args'  = filter (/= '\'') args
  let args'' = filter (/= "\"$GHCI_LOCATION\"") (words args')
  deps <- rulesHaskellCradleDependencies work_dir
  return $ makeCradleResult (ex, stde, args'') deps


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

obeliskCradleDependencies :: FilePath -> IO [FilePath]
obeliskCradleDependencies _wdir = return []

obeliskCradle :: FilePath -> Cradle
obeliskCradle wdir =
  Cradle
    { cradleRootDir  = wdir
    , cradleOptsProg = CradleAction
        { actionName = "obelisk"
        , runCradle = obeliskAction wdir
        }
    }

obeliskAction :: FilePath -> FilePath -> IO (CradleLoadResult ComponentOptions)
obeliskAction work_dir _fp = do
  (ex, args, stde) <-
      readProcessWithExitCodeInDirectory work_dir "ob" ["ide-args"] []

  o_deps <- obeliskCradleDependencies work_dir
  return (makeCradleResult (ex, stde, words args) o_deps )


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
findFile p dir = do
  b <- doesDirectoryExist dir
  if b then getFiles >>= filterM doesPredFileExist else return []
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

makeCradleResult :: (ExitCode, String, [String]) -> [FilePath] -> CradleLoadResult ComponentOptions
makeCradleResult (ex, err, gopts) deps =
  case ex of
    ExitFailure _ -> CradleFail (CradleError ex err)
    _ ->
        let compOpts = ComponentOptions gopts deps
        in CradleSuccess compOpts
