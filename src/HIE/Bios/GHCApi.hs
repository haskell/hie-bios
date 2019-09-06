{-# LANGUAGE ScopedTypeVariables, RecordWildCards, CPP #-}

module HIE.Bios.GHCApi (
    withGHC
  , withGHC'
  , withGhcT
  , initializeFlagsWithCradle
  , initializeFlagsWithCradleWithMessage
  , getDynamicFlags
  , getSystemLibDir
  , withDynFlags
  , withCmdFlags
  , setNoWarningFlags
  , setAllWarningFlags
  , setDeferTypeErrors
  , CradleError(..)
  ) where

import CoreMonad (liftIO)
import Exception (ghandle, SomeException(..), ExceptionMonad(..), throwIO, Exception(..))
import GHC (Ghc, DynFlags(..), GhcLink(..), HscTarget(..), LoadHowMuch(..), GhcMonad, GhcT)
import qualified GHC as G
import qualified DriverPhases as G
import qualified Outputable as G
import qualified MonadUtils as G
import qualified HscMain as G
import qualified GhcMake as G
import qualified Util as G
import DynFlags

import Control.Monad (void, when)
import System.Exit (exitSuccess, ExitCode(..))
import System.IO (hPutStr, hPrint, stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)

import System.Directory
import System.FilePath

import qualified HIE.Bios.Gap as Gap
import HIE.Bios.Types
import Debug.Trace
import qualified Crypto.Hash.SHA1 as H
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Base16
import Data.List

----------------------------------------------------------------

-- | Obtaining the directory for system libraries.
getSystemLibDir :: IO (Maybe FilePath)
getSystemLibDir = do
    res <- readProcess "ghc" ["--print-libdir"] []
    return $ case res of
        ""   -> Nothing
        dirn -> Just (init dirn)

----------------------------------------------------------------

-- | Converting the 'Ghc' monad to the 'IO' monad.
withGHC :: FilePath  -- ^ A target file displayed in an error message.
        -> Ghc a -- ^ 'Ghc' actions created by the Ghc utilities.
        -> IO a
withGHC file body = ghandle ignore $ withGHC' body
  where
    ignore :: SomeException -> IO a
    ignore e = do
        hPutStr stderr $ file ++ ":0:0:Error:"
        hPrint stderr e
        exitSuccess

withGHC' :: Ghc a -> IO a
withGHC' body = do
    mlibdir <- getSystemLibDir
    G.runGhc mlibdir body

withGhcT :: (Exception.ExceptionMonad m, G.MonadIO m, Monad m) => GhcT m a -> m a
withGhcT body = do
  mlibdir <- G.liftIO $ getSystemLibDir
  G.runGhcT mlibdir body

----------------------------------------------------------------

data Build = CabalPkg | SingleFile deriving Eq

initializeFlagsWithCradle ::
        (GhcMonad m)
        => FilePath -- The file we are loading it because of
        -> Cradle
        -> m ()
initializeFlagsWithCradle = initializeFlagsWithCradleWithMessage (Just G.batchMsg)

-- | Initialize the 'DynFlags' relating to the compilation of a single
-- file or GHC session according to the 'Cradle' and 'Options'
-- provided.
initializeFlagsWithCradleWithMessage ::
        (GhcMonad m)
        => Maybe G.Messager
        -> FilePath -- The file we are loading it because of
        -> Cradle
        -> m ()
initializeFlagsWithCradleWithMessage msg fp cradle = do
      (ex, err, ghcOpts) <- liftIO $ getOptions (cradleOptsProg cradle) fp
      G.pprTrace "res" (G.text (show (ex, err, ghcOpts, fp))) (return ())
      case ex of
        ExitFailure _ -> throwCradleError err
        _ -> return ()
      let compOpts = CompilerOptions ghcOpts
      liftIO $ hPrint stderr ghcOpts
      initSessionWithMessage msg compOpts

data CradleError = CradleError String deriving (Show)

instance Exception CradleError where

throwCradleError :: GhcMonad m => String -> m ()
throwCradleError = liftIO . throwIO . CradleError

----------------------------------------------------------------
cacheDir :: String
cacheDir = "haskell-ide-engine"

clearInterfaceCache :: FilePath -> IO ()
clearInterfaceCache fp = do
  cd <- getCacheDir fp
  res <- doesPathExist cd
  when res (removeDirectoryRecursive cd)

getCacheDir :: FilePath -> IO FilePath
getCacheDir fp = getXdgDirectory XdgCache (cacheDir ++ "/" ++ fp)

initSessionWithMessage :: (GhcMonad m)
            => Maybe G.Messager
            -> CompilerOptions
            -> m ()
initSessionWithMessage msg CompilerOptions {..} = do
    df <- G.getSessionDynFlags
    traceShowM (length ghcOptions)

    let opts_hash = B.unpack $ encode $ H.finalize $ H.updates H.init (map B.pack ghcOptions)
    fp <- liftIO $ getCacheDir opts_hash
    -- For now, clear the cache initially rather than persist it across
    -- sessions
    liftIO $ clearInterfaceCache opts_hash
    (df', targets) <- addCmdOpts ghcOptions df
    void $ G.setSessionDynFlags
      (disableOptimisation
      $ setIgnoreInterfacePragmas
      $ resetPackageDb
--      $ ignorePackageEnv
      $ writeInterfaceFiles (Just fp)
      $ setVerbosity 0

      $ setLinkerOptions df'
      )
    G.setLogAction (\_df _wr _s _ss _pp _m -> return ())
    G.setTargets targets
    -- Get the module graph using the function `getModuleGraph`
    mod_graph <- G.depanal [] True
    void $ G.load' LoadAllTargets msg mod_graph

----------------------------------------------------------------

-- we don't want to generate object code so we compile to bytecode
-- (HscInterpreted) which implies LinkInMemory
-- HscInterpreted
setLinkerOptions :: DynFlags -> DynFlags
setLinkerOptions df = df {
    ghcLink   = LinkInMemory
  , hscTarget = HscNothing
  , ghcMode = CompManager
  }

resetPackageDb :: DynFlags -> DynFlags
resetPackageDb df = df { pkgDatabase = Nothing }

--ignorePackageEnv :: DynFlags -> DynFlags
--ignorePackageEnv df = df { packageEnv = Just "-" }

setIgnoreInterfacePragmas :: DynFlags -> DynFlags
setIgnoreInterfacePragmas df = gopt_set df Opt_IgnoreInterfacePragmas

setVerbosity :: Int -> DynFlags -> DynFlags
setVerbosity n df = df { verbosity = n }

writeInterfaceFiles :: Maybe FilePath -> DynFlags -> DynFlags
writeInterfaceFiles Nothing df = df
writeInterfaceFiles (Just hi_dir) df = setHiDir hi_dir (gopt_set df Opt_WriteInterface)

setHiDir :: FilePath -> DynFlags -> DynFlags
setHiDir f d = d { hiDir      = Just f}


addCmdOpts :: (GhcMonad m)
           => [String] -> DynFlags -> m (DynFlags, [G.Target])
addCmdOpts cmdOpts df1 = do
  (df2, leftovers, warns) <- G.parseDynamicFlags df1 (map G.noLoc cmdOpts)
  traceShowM (map G.unLoc leftovers, length warns)

  let
     -- To simplify the handling of filepaths, we normalise all filepaths right
     -- away. Note the asymmetry of FilePath.normalise:
     --    Linux:   p/q -> p/q; p\q -> p\q
     --    Windows: p/q -> p\q; p\q -> p\q
     -- #12674: Filenames starting with a hypen get normalised from ./-foo.hs
     -- to -foo.hs. We have to re-prepend the current directory.
    normalise_hyp fp
        | strt_dot_sl && "-" `isPrefixOf` nfp = cur_dir ++ nfp
        | otherwise                           = nfp
        where
#if defined(mingw32_HOST_OS)
          strt_dot_sl = "./" `isPrefixOf` fp || ".\\" `isPrefixOf` fp
#else
          strt_dot_sl = "./" `isPrefixOf` fp
#endif
          cur_dir = '.' : [pathSeparator]
          nfp = normalise fp
    normal_fileish_paths = map (normalise_hyp . G.unLoc) leftovers
  let
   (srcs, objs) = partition_args normal_fileish_paths [] []
   df3 = df2 { ldInputs = map (FileOption "") objs ++ ldInputs df2 }
  ts <- mapM (uncurry G.guessTarget) srcs
  return (df3, ts)
    -- TODO: Need to handle these as well
    -- Ideally it requires refactoring to work in GHCi monad rather than
    -- Ghc monad and then can just use newDynFlags.
    {-
    liftIO $ G.handleFlagWarnings idflags1 warns
    when (not $ null leftovers)
        (throwGhcException . CmdLineError
         $ "Some flags have not been recognized: "
         ++ (concat . intersperse ", " $ map unLoc leftovers))
    when (interactive_only && packageFlagsChanged idflags1 idflags0) $ do
       liftIO $ hPutStrLn stderr "cannot set package flags with :seti; use :set"
    -}

----------------------------------------------------------------


----------------------------------------------------------------

-- | Return the 'DynFlags' currently in use in the GHC session.
getDynamicFlags :: IO DynFlags
getDynamicFlags = do
    mlibdir <- getSystemLibDir
    G.runGhc mlibdir G.getSessionDynFlags

withDynFlags ::
  (GhcMonad m)
  => (DynFlags -> DynFlags) -> m a -> m a
withDynFlags setFlag body = G.gbracket setup teardown (\_ -> body)
  where
    setup = do
        dflag <- G.getSessionDynFlags
        void $ G.setSessionDynFlags (setFlag dflag)
        return dflag
    teardown = void . G.setSessionDynFlags

withCmdFlags ::
  (GhcMonad m)
  => [String] -> m a ->  m a
withCmdFlags flags body = G.gbracket setup teardown (\_ -> body)
  where
    setup = do
        (dflag, _) <- G.getSessionDynFlags >>= addCmdOpts flags
        void $ G.setSessionDynFlags dflag
        return dflag
    teardown = void . G.setSessionDynFlags

-- partition_args, along with some of the other code in this file,
-- was copied from ghc/Main.hs
-- -----------------------------------------------------------------------------
-- Splitting arguments into source files and object files.  This is where we
-- interpret the -x <suffix> option, and attach a (Maybe Phase) to each source
-- file indicating the phase specified by the -x option in force, if any.
partition_args :: [String] -> [(String, Maybe G.Phase)] -> [String]
               -> ([(String, Maybe G.Phase)], [String])
partition_args [] srcs objs = (reverse srcs, reverse objs)
partition_args ("-x":suff:args) srcs objs
  | "none" <- suff      = partition_args args srcs objs
  | G.StopLn <- phase     = partition_args args srcs (slurp ++ objs)
  | otherwise           = partition_args rest (these_srcs ++ srcs) objs
        where phase = G.startPhase suff
              (slurp,rest) = break (== "-x") args
              these_srcs = zip slurp (repeat (Just phase))
partition_args (arg:args) srcs objs
  | looks_like_an_input arg = partition_args args ((arg,Nothing):srcs) objs
  | otherwise               = partition_args args srcs (arg:objs)

    {-
      We split out the object files (.o, .dll) and add them
      to ldInputs for use by the linker.
      The following things should be considered compilation manager inputs:
       - haskell source files (strings ending in .hs, .lhs or other
         haskellish extension),
       - module names (not forgetting hierarchical module names),
       - things beginning with '-' are flags that were not recognised by
         the flag parser, and we want them to generate errors later in
         checkOptions, so we class them as source files (#5921)
       - and finally we consider everything without an extension to be
         a comp manager input, as shorthand for a .hs or .lhs filename.
      Everything else is considered to be a linker object, and passed
      straight through to the linker.
    -}
looks_like_an_input :: String -> Bool
looks_like_an_input m =  G.isSourceFilename m
                      || G.looksLikeModuleName m
                      || "-" `isPrefixOf` m
                      || not (hasExtension m)


----------------------------------------------------------------

setDeferTypeErrors :: DynFlags -> DynFlags
setDeferTypeErrors
  = foldDFlags (flip wopt_set) [Opt_WarnTypedHoles, Opt_WarnDeferredTypeErrors, Opt_WarnDeferredOutOfScopeVariables]
  . foldDFlags setGeneralFlag' [Opt_DeferTypedHoles, Opt_DeferTypeErrors, Opt_DeferOutOfScopeVariables]

foldDFlags :: (a -> DynFlags -> DynFlags) -> [a] -> DynFlags -> DynFlags
foldDFlags f xs x = foldr f x xs

-- | Set 'DynFlags' equivalent to "-w:".
setNoWarningFlags :: DynFlags -> DynFlags
setNoWarningFlags df = df { warningFlags = Gap.emptyWarnFlags}

-- | Set 'DynFlags' equivalent to "-Wall".
setAllWarningFlags :: DynFlags -> DynFlags
setAllWarningFlags df = df { warningFlags = allWarningFlags }

disableOptimisation :: DynFlags -> DynFlags
disableOptimisation df = updOptLevel 0 df

{-# NOINLINE allWarningFlags #-}
allWarningFlags :: Gap.WarnFlags
allWarningFlags = unsafePerformIO $ do
    mlibdir <- getSystemLibDir
    G.runGhcT mlibdir $ do
        df <- G.getSessionDynFlags
        (df', _) <- addCmdOpts ["-Wall"] df
        return $ G.warningFlags df'
