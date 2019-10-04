{-# LANGUAGE RecordWildCards, CPP #-}
module HIE.Bios.Environment (initSession, getSystemLibDir, addCmdOpts, getDynamicFlags) where

import CoreMonad (liftIO)
import GHC (DynFlags(..), GhcLink(..), HscTarget(..), GhcMonad)
import qualified GHC as G
import qualified DriverPhases as G
import qualified Util as G
import DynFlags

import Control.Monad (void, when)

import System.Process (readProcess)
import System.Directory
import System.FilePath

import qualified Crypto.Hash.SHA1 as H
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Base16
import Data.List

import HIE.Bios.Types

initSession :: (GhcMonad m)
    => ComponentOptions
    -> m [G.Target]
initSession  ComponentOptions {..} = do
    df <- G.getSessionDynFlags

    let opts_hash = B.unpack $ encode $ H.finalize $ H.updates H.init (map B.pack componentOptions)
    fp <- liftIO $ getCacheDir opts_hash
    (df', targets) <- addCmdOpts componentOptions df
    void $ G.setSessionDynFlags
        (disableOptimisation
        $ setIgnoreInterfacePragmas
        $ resetPackageDb
        -- --  $ ignorePackageEnv
        $ writeInterfaceFiles (Just fp)
        $ setVerbosity 0

        $ setLinkerOptions df'
        )
    G.setLogAction (\_df _wr _s _ss _pp _m -> return ())
#if __GLASGOW_HASKELL__ < 806
        (\_df -> return ())
#endif

    return targets

----------------------------------------------------------------

-- | Obtaining the directory for system libraries.
getSystemLibDir :: IO (Maybe FilePath)
getSystemLibDir = do
    res <- readProcess "ghc" ["--print-libdir"] []
    return $ case res of
        ""   -> Nothing
        dirn -> Just (init dirn)

----------------------------------------------------------------


cacheDir :: String
cacheDir = "hie-bios"

{-
-- Back in the day we used to clear the cache at the start of each session,
-- however, it's not really necessary as
-- 1. There is one cache dir for any change in options.
-- 2. Interface files are resistent to bad option changes anyway.
clearInterfaceCache :: FilePath -> IO ()
clearInterfaceCache fp = do
  cd <- getCacheDir fp
  res <- doesPathExist cd
  when res (removeDirectoryRecursive cd)
-}

getCacheDir :: FilePath -> IO FilePath
getCacheDir fp = getXdgDirectory XdgCache (cacheDir </> fp)

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
  (df2, leftovers, _warns) <- G.parseDynamicFlags df1 (map G.noLoc cmdOpts)

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

-- | Return the 'DynFlags' currently in use in the GHC session.
getDynamicFlags :: IO DynFlags
getDynamicFlags = do
    mlibdir <- getSystemLibDir
    G.runGhc mlibdir G.getSessionDynFlags


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

-- --------------------------------------------------------

disableOptimisation :: DynFlags -> DynFlags
disableOptimisation df = updOptLevel 0 df
