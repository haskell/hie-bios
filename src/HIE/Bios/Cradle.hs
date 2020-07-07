{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
module HIE.Bios.Cradle (
      findCradle
    , loadCradle
    , loadCustomCradle
    , loadImplicitCradle
    , defaultCradle
    , isCabalCradle
    , isStackCradle
    , isDirectCradle
    , isBiosCradle
    , isNoneCradle
    , isMultiCradle
    , isDefaultCradle
    , isOtherCradle
    , getCradle
    , readProcessWithOutputFile
    , makeCradleResult
  ) where

import Control.Exception (handleJust)
import qualified Data.Yaml as Yaml
import Data.Void
import System.Process
import System.Exit
import HIE.Bios.Types hiding (ActionName(..))
import qualified HIE.Bios.Types as Types
import HIE.Bios.Config
import HIE.Bios.Environment (getCacheDir)
import System.Directory hiding (findFile)
import Control.Monad.Trans.Maybe
import System.FilePath
import Control.Monad
import System.Info.Extra
import Control.Monad.IO.Class
import System.Environment
import Control.Applicative ((<|>), optional)
import System.IO.Temp
import System.IO.Error (isPermissionError)
import Data.List
import Data.Ord (Down(..))

import System.PosixCompat.Files
import HIE.Bios.Wrappers
import System.IO
import Control.DeepSeq

import Data.Conduit.Process
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit as C
import qualified Data.Conduit.Text as C
import qualified Data.Text as T
import           Data.Maybe (fromMaybe, maybeToList)
import           GHC.Fingerprint (fingerprintString)

----------------------------------------------------------------

-- | Given root\/foo\/bar.hs, return root\/hie.yaml, or wherever the yaml file was found.
findCradle :: FilePath -> IO (Maybe FilePath)
findCradle wfile = do
    let wdir = takeDirectory wfile
    runMaybeT (yamlConfig wdir)

-- | Given root\/hie.yaml load the Cradle.
loadCradle :: FilePath -> IO (Cradle Void)
loadCradle = loadCradleWithOpts Types.defaultCradleOpts absurd

loadCustomCradle :: Yaml.FromJSON b => (b -> Cradle a) -> FilePath -> IO (Cradle a)
loadCustomCradle = loadCradleWithOpts Types.defaultCradleOpts

-- | Given root\/foo\/bar.hs, load an implicit cradle
loadImplicitCradle :: Show a => FilePath -> IO (Cradle a)
loadImplicitCradle wfile = do
  let wdir = takeDirectory wfile
  cfg <- runMaybeT (implicitConfig wdir)
  return $ case cfg of
    Just bc -> getCradle absurd bc
    Nothing -> defaultCradle wdir

-- | Finding 'Cradle'.
--   Find a cabal file by tracing ancestor directories.
--   Find a sandbox according to a cabal sandbox config
--   in a cabal directory.
loadCradleWithOpts :: (Yaml.FromJSON b) => CradleOpts -> (b -> Cradle a) -> FilePath -> IO (Cradle a)
loadCradleWithOpts _copts buildCustomCradle wfile = do
    cradleConfig <- readCradleConfig wfile
    return $ getCradle buildCustomCradle (cradleConfig, takeDirectory wfile)

getCradle :: (b -> Cradle a) -> (CradleConfig b, FilePath) -> Cradle a
getCradle buildCustomCradle (cc, wdir) = addCradleDeps cradleDeps $ case cradleType cc of
    Cabal mc -> cabalCradle wdir mc
    CabalMulti ms ->
      getCradle buildCustomCradle $
        (CradleConfig cradleDeps
          (Multi [(p, CradleConfig [] (Cabal (Just c))) | (p, c) <- ms])
        , wdir)
    Stack mc -> stackCradle wdir mc
    StackMulti ms ->
      getCradle buildCustomCradle $
        (CradleConfig cradleDeps
          (Multi [(p, CradleConfig [] (Stack (Just c))) | (p, c) <- ms])
        , wdir)
 --   Bazel -> rulesHaskellCradle wdir
 --   Obelisk -> obeliskCradle wdir
    Bios bios deps  -> biosCradle wdir bios deps
    Direct xs -> directCradle wdir xs
    None      -> noneCradle wdir
    Multi ms  -> multiCradle buildCustomCradle wdir ms
    Other a _ -> buildCustomCradle a
    where
      cradleDeps = cradleDependencies cc

addCradleDeps :: [FilePath] -> Cradle a -> Cradle a
addCradleDeps deps c =
  c { cradleOptsProg = addActionDeps (cradleOptsProg c) }
  where
    addActionDeps :: CradleAction a -> CradleAction a
    addActionDeps ca =
      ca { runCradle = \l fp ->
            runCradle ca l fp
              >>= \case
                CradleSuccess (ComponentOptions os' dir ds) ->
                  pure $ CradleSuccess (ComponentOptions os' dir (ds `union` deps))
                CradleFail err ->
                  pure $ CradleFail
                    (err { cradleErrorDependencies = cradleErrorDependencies err `union` deps })
                CradleNone -> pure CradleNone
         }


implicitConfig :: FilePath -> MaybeT IO (CradleConfig a, FilePath)
implicitConfig fp = do
  (crdType, wdir) <- implicitConfig' fp
  return (CradleConfig [] crdType, wdir)

implicitConfig' :: FilePath -> MaybeT IO (CradleType a, FilePath)
implicitConfig' fp = (\wdir ->
         (Bios (Program $ wdir </> ".hie-bios") Nothing, wdir)) <$> biosWorkDir fp
  --   <|> (Obelisk,) <$> obeliskWorkDir fp
  --   <|> (Bazel,) <$> rulesHaskellWorkDir fp
     <|> (stackExecutable >> (Stack Nothing,) <$> stackWorkDir fp)
     <|> ((Cabal Nothing,) <$> cabalWorkDir fp)


yamlConfig :: FilePath ->  MaybeT IO FilePath
yamlConfig fp = do
  configDir <- yamlConfigDirectory fp
  return (configDir </> configFileName)

yamlConfigDirectory :: FilePath -> MaybeT IO FilePath
yamlConfigDirectory = findFileUpwards (configFileName ==)

readCradleConfig :: Yaml.FromJSON b => FilePath -> IO (CradleConfig b)
readCradleConfig yamlHie = do
  cfg  <- liftIO $ readConfig yamlHie
  return (cradle cfg)

configFileName :: FilePath
configFileName = "hie.yaml"

---------------------------------------------------------------

isCabalCradle :: Cradle a -> Bool
isCabalCradle crdl = case actionName (cradleOptsProg crdl) of
  Types.Cabal -> True
  _ -> False

isStackCradle :: Cradle a -> Bool
isStackCradle crdl = case actionName (cradleOptsProg crdl) of
  Types.Stack -> True
  _ -> False

isDirectCradle :: Cradle a -> Bool
isDirectCradle crdl = case actionName (cradleOptsProg crdl) of
  Types.Direct -> True
  _ -> False

isBiosCradle :: Cradle a -> Bool
isBiosCradle crdl = case actionName (cradleOptsProg crdl) of
  Types.Bios -> True
  _ -> False

isMultiCradle :: Cradle a -> Bool
isMultiCradle crdl = case actionName (cradleOptsProg crdl) of
  Types.Multi -> True
  _ -> False

isNoneCradle :: Cradle a -> Bool
isNoneCradle crdl = case actionName (cradleOptsProg crdl) of
  Types.None -> True
  _ -> False

isDefaultCradle :: Cradle a -> Bool
isDefaultCradle crdl = case actionName (cradleOptsProg crdl) of
  Types.Default -> True
  _ -> False

isOtherCradle :: Cradle a -> Bool
isOtherCradle crdl = case actionName (cradleOptsProg crdl) of
  Types.Other _ -> True
  _ -> False

---------------------------------------------------------------

-- | Default cradle has no special options, not very useful for loading
-- modules.
defaultCradle :: FilePath -> Cradle a
defaultCradle cur_dir =
  Cradle
    { cradleRootDir = cur_dir
    , cradleOptsProg = CradleAction
        { actionName = Types.Default
        , runCradle = \_ _ ->
            return (CradleSuccess (ComponentOptions [] cur_dir []))
        , runGhcCmd = runGhcCmdOnPath cur_dir
        }
    }

---------------------------------------------------------------
-- | The none cradle tells us not to even attempt to load a certain directory

noneCradle :: FilePath -> Cradle a
noneCradle cur_dir =
  Cradle
    { cradleRootDir = cur_dir
    , cradleOptsProg = CradleAction
        { actionName = Types.None
        , runCradle = \_ _ -> return CradleNone
        , runGhcCmd = \_   -> return CradleNone
        }
    }

---------------------------------------------------------------
-- | The multi cradle selects a cradle based on the filepath

multiCradle :: (b -> Cradle a) -> FilePath -> [(FilePath, CradleConfig b)] -> Cradle a
multiCradle buildCustomCradle cur_dir cs =
  Cradle
    { cradleRootDir  = cur_dir
    , cradleOptsProg = CradleAction
        { actionName = multiActionName
        , runCradle  = \l fp -> canonicalizePath fp >>= multiAction buildCustomCradle cur_dir cs l
        , runGhcCmd = \args ->
            -- We're being lazy here and just returning the ghc path for the
            -- first non-none cradle. This shouldn't matter in practice: all
            -- sub cradles should be using the same ghc version!
            case filter (not . isNoneCradleConfig) $ map snd cs of
              [] -> return CradleNone
              (cfg:_) -> flip runGhcCmd args $ cradleOptsProg $
                getCradle buildCustomCradle (cfg, cur_dir)
        }
    }
  where
    cfgs = map snd cs

    multiActionName
      | all (\c -> isStackCradleConfig c || isNoneCradleConfig c) cfgs
      = Types.Stack
      | all (\c -> isCabalCradleConfig c || isNoneCradleConfig c) cfgs
      = Types.Cabal
      | otherwise
      = Types.Multi

    isStackCradleConfig cfg = case cradleType cfg of
      Stack{}      -> True
      StackMulti{} -> True
      _            -> False

    isCabalCradleConfig cfg = case cradleType cfg of
      Cabal{}      -> True
      CabalMulti{} -> True
      _            -> False

    isNoneCradleConfig cfg = case cradleType cfg of
      None -> True
      _    -> False

multiAction ::  forall b a
            . (b -> Cradle a)
            -> FilePath
            -> [(FilePath, CradleConfig b)]
            -> LoggingFunction
            -> FilePath
            -> IO (CradleLoadResult ComponentOptions)
multiAction buildCustomCradle cur_dir cs l cur_fp =
    selectCradle =<< canonicalizeCradles

  where
    err_msg = ["Multi Cradle: No prefixes matched"
              , "pwd: " ++ cur_dir
              , "filepath: " ++ cur_fp
              , "prefixes:"
              ] ++ [show (pf, cradleType cc) | (pf, cc) <- cs]

    -- Canonicalize the relative paths present in the multi-cradle and
    -- also order the paths by most specific first. In the cradle selection
    -- function we want to choose the most specific cradle possible.
    canonicalizeCradles :: IO [(FilePath, CradleConfig b)]
    canonicalizeCradles =
      sortOn (Down . fst)
        <$> mapM (\(p, c) -> (,c) <$> (canonicalizePath (cur_dir </> p))) cs

    selectCradle [] =
      return (CradleFail (CradleError [] ExitSuccess err_msg))
    selectCradle ((p, c): css) =
        if p `isPrefixOf` cur_fp
          then runCradle
                  (cradleOptsProg (getCradle buildCustomCradle (c, cur_dir)))
                  l
                  cur_fp
          else selectCradle css


-------------------------------------------------------------------------

directCradle :: FilePath -> [String] -> Cradle a
directCradle wdir args =
  Cradle
    { cradleRootDir = wdir
    , cradleOptsProg = CradleAction
        { actionName = Types.Direct
        , runCradle = \_ _ ->
            return (CradleSuccess (ComponentOptions args wdir []))
        , runGhcCmd = runGhcCmdOnPath wdir
        }
    }

-------------------------------------------------------------------------


-- | Find a cradle by finding an executable `hie-bios` file which will
-- be executed to find the correct GHC options to use.
biosCradle :: FilePath -> Callable -> Maybe Callable -> Cradle a
biosCradle wdir biosCall biosDepsCall =
  Cradle
    { cradleRootDir    = wdir
    , cradleOptsProg   = CradleAction
        { actionName = Types.Bios
        , runCradle = biosAction wdir biosCall biosDepsCall
        , runGhcCmd = runGhcCmdOnPath wdir
        }
    }

biosWorkDir :: FilePath -> MaybeT IO FilePath
biosWorkDir = findFileUpwards (".hie-bios" ==)

biosDepsAction :: LoggingFunction -> FilePath -> Maybe Callable -> IO [FilePath]
biosDepsAction l wdir (Just biosDepsCall) = do
  biosDeps' <- callableToProcess biosDepsCall Nothing
  (ex, sout, serr, args) <- readProcessWithOutputFile l wdir biosDeps'
  case ex of
    ExitFailure _ ->  error $ show (ex, sout, serr)
    ExitSuccess -> return args
biosDepsAction _ _ Nothing = return []

biosAction :: FilePath
           -> Callable
           -> Maybe Callable
           -> LoggingFunction
           -> FilePath
           -> IO (CradleLoadResult ComponentOptions)
biosAction wdir bios bios_deps l fp = do
  bios' <- callableToProcess bios (Just fp)
  (ex, _stdo, std, res) <- readProcessWithOutputFile l wdir bios'
  deps <- biosDepsAction l wdir bios_deps
        -- Output from the program should be written to the output file and
        -- delimited by newlines.
        -- Execute the bios action and add dependencies of the cradle.
        -- Removes all duplicates.
  return $ makeCradleResult (ex, std, wdir, res) deps

callableToProcess :: Callable -> Maybe String -> IO CreateProcess
callableToProcess (Command shellCommand) file = do
  old_env <- getEnvironment
  return $ (shell shellCommand) { env = (: old_env) <$> (,) hieBiosArg <$> file }
    where
      hieBiosArg = "HIE_BIOS_ARG"
callableToProcess (Program path) file = do
  canon_path <- canonicalizePath path
  return $ proc canon_path (maybeToList file)

------------------------------------------------------------------------
-- |Cabal Cradle
-- Works for new-build by invoking `v2-repl`.
cabalCradle :: FilePath -> Maybe String -> Cradle a
cabalCradle wdir mc =
  Cradle
    { cradleRootDir    = wdir
    , cradleOptsProg   = CradleAction
        { actionName = Types.Cabal
        , runCradle = cabalAction wdir mc
        , runGhcCmd = \args -> do
            -- Workaround for a cabal-install bug on 3.0.0.0:
            -- ./dist-newstyle/tmp/environment.-24811: createDirectory: does not exist (No such file or directory)
            -- (It's ok to pass 'dist-newstyle' here, as it can only be changed
            -- with the --builddir flag and not cabal.project, which we aren't
            -- using in our call to v2-exec)
            createDirectoryIfMissing True (wdir </> "dist-newstyle" </> "tmp")
            -- Need to pass -v0 otherwise we get "resolving dependencies..."
            readProcessWithCwd
              wdir "cabal" (["v2-exec", "ghc", "-v0", "--"] ++ args) ""
        }
    }

-- | @'cabalCradleDependencies' rootDir componentDir@.
-- Compute the dependencies of the cabal cradle based
-- on the cradle root and the component directory.
--
-- Directory 'componentDir' is a sub-directory where we look for
-- package specific cradle dependencies, such as a '.cabal' file.
--
-- Found dependencies are relative to 'rootDir'.
cabalCradleDependencies :: FilePath -> FilePath -> IO [FilePath]
cabalCradleDependencies rootDir componentDir = do
    let relFp = makeRelative rootDir componentDir
    cabalFiles' <- findCabalFiles componentDir
    let cabalFiles = map (relFp </>) cabalFiles'
    return $ map normalise $ cabalFiles ++ ["cabal.project", "cabal.project.local"]

-- |Find .cabal files in the given directory.
--
-- Might return multiple results, as we can not know in advance
-- which one is important to the user.
findCabalFiles :: FilePath -> IO [FilePath]
findCabalFiles wdir = do
  dirContent <- listDirectory wdir
  return $ filter ((== ".cabal") . takeExtension) dirContent


processCabalWrapperArgs :: [String] -> Maybe (FilePath, [String])
processCabalWrapperArgs args =
    case args of
        (dir: ghc_args) ->
            let final_args =
                    removeVerbosityOpts
                    $ removeRTS
                    $ removeInteractive
                    $ ghc_args
            in Just (dir, final_args)
        _ -> Nothing

-- | GHC process information.
-- Consists of the filepath to the ghc executable and
-- arguments to the executable.
type GhcProc = (FilePath, [String])

-- | Generate a fake GHC that can be passed to cabal
-- when run with --interactive, it will print out its
-- command-line arguments and exit
withCabalWrapperTool :: GhcProc -> FilePath -> (FilePath -> IO a) -> IO a
withCabalWrapperTool (ghcPath, ghcArgs) wdir k = do
  if isWindows
    then do
      cacheDir <- getCacheDir ""
      let srcHash = show (fingerprintString cabalWrapperHs)
      let wrapper_name = "wrapper-" ++ srcHash
      let wrapper_fp = cacheDir </> wrapper_name <.> "exe"
      exists <- doesFileExist wrapper_fp
      unless exists $ withSystemTempDirectory "hie-bios" $ \ tmpDir -> do
          createDirectoryIfMissing True cacheDir
          let wrapper_hs = cacheDir </> wrapper_name <.> "hs"
          writeFile wrapper_hs cabalWrapperHs
          let ghc = (proc ghcPath $
                      ghcArgs ++ ["-rtsopts=ignore", "-outputdir", tmpDir, "-o", wrapper_fp, wrapper_hs])
                      { cwd = Just wdir }
          readCreateProcess ghc "" >>= putStr
      setMode wrapper_fp
      k wrapper_fp
    else withSystemTempFile "bios-wrapper"
            (\loc h -> do
                        hPutStr h cabalWrapper
                        hClose h
                        setMode loc
                        k loc)

  where
    setMode wrapper_fp = setFileMode wrapper_fp accessModes


cabalAction :: FilePath -> Maybe String -> LoggingFunction -> FilePath -> IO (CradleLoadResult ComponentOptions)
cabalAction work_dir mc l fp = do
  withCabalWrapperTool ("ghc", []) work_dir $ \wrapper_fp -> do
    let cab_args = ["v2-repl", "--with-compiler", wrapper_fp, fromMaybe (fixTargetPath fp) mc]
    (ex, output, stde, args) <-
      readProcessWithOutputFile l work_dir (proc "cabal" cab_args)
    case processCabalWrapperArgs args of
        Nothing -> do
          -- Best effort. Assume the working directory is the
          -- the root of the component, so we are right in trivial cases at least.
          deps <- cabalCradleDependencies work_dir work_dir
          pure $ CradleFail (CradleError deps ex
                    ["Failed to parse result of calling cabal"
                     , unlines output
                     , unlines stde
                     , unlines args])
        Just (componentDir, final_args) -> do
          deps <- cabalCradleDependencies work_dir componentDir
          pure $ makeCradleResult (ex, stde, componentDir, final_args) deps
  where
    -- Need to make relative on Windows, due to a Cabal bug with how it
    -- parses file targets with a C: drive in it
    fixTargetPath x
      | isWindows && hasDrive x = makeRelative work_dir x
      | otherwise = x

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


cabalWorkDir :: FilePath -> MaybeT IO FilePath
cabalWorkDir wdir =
      findFileUpwards (== "cabal.project") wdir
  <|> findFileUpwards (\fp -> takeExtension fp == ".cabal") wdir

------------------------------------------------------------------------
-- | Stack Cradle
-- Works for by invoking `stack repl` with a wrapper script
stackCradle :: FilePath -> Maybe String -> Cradle a
stackCradle wdir mc =
  Cradle
    { cradleRootDir    = wdir
    , cradleOptsProg   = CradleAction
        { actionName = Types.Stack
        , runCradle = stackAction wdir mc
        , runGhcCmd = \args ->
            readProcessWithCwd
              wdir "stack" (["exec", "--silent", "ghc", "--"] <> args) ""
        }
    }

-- | @'stackCradleDependencies' rootDir componentDir@.
-- Compute the dependencies of the stack cradle based
-- on the cradle root and the component directory.
--
-- Directory 'componentDir' is a sub-directory where we look for
-- package specific cradle dependencies, such as 'package.yaml' and
-- a '.cabal' file.
--
-- Found dependencies are relative to 'rootDir'.
stackCradleDependencies :: FilePath -> FilePath -> IO [FilePath]
stackCradleDependencies wdir componentDir = do
  let relFp = makeRelative wdir componentDir
  cabalFiles' <- findCabalFiles componentDir
  let cabalFiles = map (relFp </>) cabalFiles'
  return $ map normalise $ cabalFiles ++ [relFp </> "package.yaml", "stack.yaml"]

stackAction :: FilePath -> Maybe String -> LoggingFunction -> FilePath -> IO (CradleLoadResult ComponentOptions)
stackAction work_dir mc l _fp = do
  let ghcProcArgs = ("stack", ["exec", "ghc", "--"])
  -- Same wrapper works as with cabal
  withCabalWrapperTool ghcProcArgs work_dir $ \wrapper_fp -> do
    (ex1, _stdo, stde, args) <-
      readProcessWithOutputFile l work_dir $
        proc "stack" $ ["repl", "--no-nix-pure", "--with-ghc", wrapper_fp]
                       ++ [ comp | Just comp <- [mc] ]
    (ex2, pkg_args, stdr, _) <-
      readProcessWithOutputFile l work_dir $
        proc "stack" ["path", "--ghc-package-path"]
    let split_pkgs = concatMap splitSearchPath pkg_args
        pkg_ghc_args = concatMap (\p -> ["-package-db", p] ) split_pkgs
    case processCabalWrapperArgs args of
        Nothing -> do
          -- Best effort. Assume the working directory is the
          -- the root of the component, so we are right in trivial cases at least.
          deps <- stackCradleDependencies work_dir work_dir
          pure $ CradleFail
                    (CradleError deps ex1 $
                      [ "Failed to parse result of calling stack" ]
                      ++ stde
                      ++ args
                    )

        Just (componentDir, ghc_args) -> do
          deps <- stackCradleDependencies work_dir componentDir
          pure $ makeCradleResult
                    ( combineExitCodes [ex1, ex2]
                    , stde ++ stdr, componentDir
                    , ghc_args ++ pkg_ghc_args
                    )
                    deps

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

{-
-- Support removed for 0.3 but should be added back in the future
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
      readProcessWithOutputFile work_dir wrapper_fp [rel_path] []
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
      readProcessWithOutputFile work_dir "ob" ["ide-args"] []

  o_deps <- obeliskCradleDependencies work_dir
  return (makeCradleResult (ex, stde, words args) o_deps )

-}
------------------------------------------------------------------------------
-- Utilities


-- | Searches upwards for the first directory containing a file to match
-- the predicate.
findFileUpwards :: (FilePath -> Bool) -> FilePath -> MaybeT IO FilePath
findFileUpwards p dir = do
  cnts <-
    liftIO
    $ handleJust
        -- Catch permission errors
        (\(e :: IOError) -> if isPermissionError e then Just [] else Nothing)
        pure
        (findFile p dir)

  case cnts of
    [] | dir' == dir -> fail "No cabal files"
            | otherwise   -> findFileUpwards p dir'
    _ : _ -> return dir
  where dir' = takeDirectory dir

-- | Sees if any file in the directory matches the predicate
findFile :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
findFile p dir = do
  b <- doesDirectoryExist dir
  if b then getFiles >>= filterM doesPredFileExist else return []
  where
    getFiles = filter p <$> getDirectoryContents dir
    doesPredFileExist file = doesFileExist $ dir </> file

-- | Call a given process.
-- * A special file is created for the process to write to, the process can discover the name of
-- the file by reading the @HIE_BIOS_OUTPUT@ environment variable. The contents of this file is
-- returned by the function.
-- * The logging function is called every time the process emits anything to stdout or stderr.
-- it can be used to report progress of the process to a user.
-- * The process is executed in the given directory.
readProcessWithOutputFile
  :: LoggingFunction -- ^ Output of the process is streamed into this function.
  -> FilePath -- ^ Working directory. Process is executed in this directory.
  -> CreateProcess -- ^ Parameters for the process to be executed.
  -> IO (ExitCode, [String], [String], [String])
readProcessWithOutputFile l work_dir cp = do
  old_env <- getEnvironment

  withHieBiosOutput old_env $ \output_file -> do
    -- Pipe stdout directly into the logger
    let process = cp { env = Just
                              $ (hieBiosOutput, output_file)
                              : (fromMaybe old_env (env cp)),
                       cwd = Just work_dir
                      }

    -- Windows line endings are not converted so you have to filter out `'r` characters
    let  loggingConduit = (C.decodeUtf8  C..| C.lines C..| C.filterE (/= '\r')  C..| C.map T.unpack C..| C.iterM l C..| C.sinkList)
    (ex, stdo, stde) <- sourceProcessWithStreams process mempty loggingConduit loggingConduit
    res <- withFile output_file ReadMode $ \handle -> do
             hSetBuffering handle LineBuffering
             !res <- force <$> hGetContents handle
             return res

    return (ex, stdo, stde, lines (filter (/= '\r') res))

    where
      withHieBiosOutput :: [(String,String)] -> (FilePath -> IO a) -> IO a
      withHieBiosOutput env' action = do
        let mbHieBiosOut = lookup hieBiosOutput env'
        case mbHieBiosOut of
          Just file@(_:_) -> action file
          _ -> withSystemTempFile "hie-bios" $
                 \ file h -> hClose h >> action file

      hieBiosOutput = "HIE_BIOS_OUTPUT"

makeCradleResult :: (ExitCode, [String], FilePath, [String]) -> [FilePath] -> CradleLoadResult ComponentOptions
makeCradleResult (ex, err, componentDir, gopts) deps =
  case ex of
    ExitFailure _ -> CradleFail (CradleError deps ex err)
    _ ->
        let compOpts = ComponentOptions gopts componentDir deps
        in CradleSuccess compOpts

-- | Calls @ghc --print-libdir@, with just whatever's on the PATH.
runGhcCmdOnPath :: FilePath -> [String] -> IO (CradleLoadResult String)
runGhcCmdOnPath wdir args = readProcessWithCwd wdir "ghc" args ""
  -- case mResult of
  --   Nothing

-- | Wrapper around 'readCreateProcess' that sets the working directory
readProcessWithCwd :: FilePath -> FilePath -> [String] -> String -> IO (CradleLoadResult String)
readProcessWithCwd dir cmd args stdi = do
  let createProc = (proc cmd args) { cwd = Just dir }
  mResult <- optional $ readCreateProcessWithExitCode createProc stdi
  case mResult of
    Just (ExitSuccess, stdo, _) -> pure $ CradleSuccess stdo
    Just (exitCode, stdo, stde) -> pure $ CradleFail $
      CradleError [] exitCode ["Error when calling " <> cmd <> " " <> unwords args, stdo, stde]
    Nothing -> pure $ CradleFail $
      CradleError [] ExitSuccess ["Couldn't execute " <> cmd <> " " <> unwords args]
