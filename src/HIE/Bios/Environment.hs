{-# LANGUAGE RecordWildCards, CPP #-}
module HIE.Bios.Environment (initSession, getRuntimeGhcLibDir, getRuntimeGhcVersion, makeDynFlagsAbsolute, makeTargetsAbsolute, getCacheDir, addCmdOpts) where

import CoreMonad (liftIO)
import GHC (GhcMonad)
import qualified GHC as G
import qualified DriverPhases as G
import qualified Util as G
import DynFlags
import qualified HIE.Bios.Ghc.Gap as Gap

import Control.Applicative
import Control.Monad (void)

import System.Directory
import System.FilePath
import System.Environment (lookupEnv)

import qualified Crypto.Hash.SHA1 as H
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Base16
import Data.List
import Data.Char (isSpace)
import Text.ParserCombinators.ReadP hiding (optional)
import HIE.Bios.Types
import HIE.Bios.Ghc.Gap

-- | Start a GHC session and set some sensible options for tooling to use.
-- Creates a folder in the cache directory to cache interface files to make
-- reloading faster.
initSession :: (GhcMonad m)
    => ComponentOptions
    -> m [G.Target]
initSession  ComponentOptions {..} = do
    df <- G.getSessionDynFlags
    -- Create a unique folder per set of different GHC options, assuming that each different set of
    -- GHC options will create incompatible interface files.
    let opts_hash = B.unpack $ encode $ H.finalize $ H.updates H.init (map B.pack componentOptions)
    cache_dir <- liftIO $ getCacheDir opts_hash
    -- Add the user specified options to a fresh GHC session.
    (df', targets) <- addCmdOpts componentOptions df
    let df'' = makeDynFlagsAbsolute componentRoot df'
    void $ G.setSessionDynFlags
        (disableOptimisation -- Compile with -O0 as we are not going to produce object files.
        $ setIgnoreInterfacePragmas            -- Ignore any non-essential information in interface files such as unfoldings changing.
        $ writeInterfaceFiles (Just cache_dir) -- Write interface files to the cache
        $ setVerbosity 0                       -- Set verbosity to zero just in case the user specified `-vx` in the options.
        $ (if dynamicGhc then updateWays . addWay' WayDyn else id) -- Add dynamic way if GHC is built with dynamic linking
        $ setLinkerOptions df''                 -- Set `-fno-code` to avoid generating object files, unless we have to.
        )

    let targets' = makeTargetsAbsolute componentRoot targets
    -- Unset the default log action to avoid output going to stdout.
    unsetLogAction
    return targets'

----------------------------------------------------------------

makeTargetsAbsolute :: FilePath -> [G.Target] -> [G.Target]
makeTargetsAbsolute wdir = map (\target -> target {G.targetId = makeTargetIdAbsolute wdir (G.targetId target)})

makeTargetIdAbsolute :: FilePath -> G.TargetId -> G.TargetId
makeTargetIdAbsolute wdir (G.TargetFile fp phase) = G.TargetFile (wdir </> fp) phase
makeTargetIdAbsolute _ tid = tid

----------------------------------------------------------------

-- | @getRuntimeGhcLibDir cradle@ will give you the ghc libDir:
-- __do not__ use 'runGhcCmd' directly.
--
--
-- Obtains libdir by calling 'runCradleGhc' on the provided cradle.
getRuntimeGhcLibDir :: Cradle a
                    -> IO (CradleLoadResult FilePath)
getRuntimeGhcLibDir cradle = fmap (fmap trim) $
      runGhcCmd (cradleOptsProg cradle) ["--print-libdir"]

-- | Gets the version of ghc used when compiling the cradle. It is based off of
-- 'getRuntimeGhcLibDir'. If it can't work out the verison reliably, it will
-- return a 'CradleError'
getRuntimeGhcVersion :: Cradle a
                     -> IO (CradleLoadResult String)
getRuntimeGhcVersion cradle =
  fmap (fmap trim) $ runGhcCmd (cradleOptsProg cradle) ["--numeric-version"]

----------------------------------------------------------------

-- | What to call the cache directory in the cache folder.
cacheDir :: String
cacheDir = "hie-bios"

{- |
Back in the day we used to clear the cache at the start of each session,
however, it's not really necessary as
1. There is one cache dir for any change in options.
2. Interface files are resistent to bad option changes anyway.

> clearInterfaceCache :: FilePath -> IO ()
> clearInterfaceCache fp = do
>   cd <- getCacheDir fp
>   res <- doesPathExist cd
>   when res (removeDirectoryRecursive cd)
-}

-- | Prepends the cache directory used by the library to the supplied file path.
-- It tries to use the path under the environment variable `$HIE_BIOS_CACHE_DIR`
-- and falls back to the standard `$XDG_CACHE_HOME/hie-bios` if the former is not set
getCacheDir :: FilePath -> IO FilePath
getCacheDir fp = do
  mbEnvCacheDirectory <- lookupEnv "HIE_BIOS_CACHE_DIR"
  cacheBaseDir <- maybe (getXdgDirectory XdgCache cacheDir) return
                         mbEnvCacheDirectory
  return (cacheBaseDir </> fp)

----------------------------------------------------------------

-- we don't want to generate object code so we compile to bytecode
-- (HscInterpreted) which implies LinkInMemory
-- HscInterpreted
setLinkerOptions :: DynFlags -> DynFlags
setLinkerOptions df = setNoCode $ df {
    ghcLink   = LinkInMemory
  , ghcMode = CompManager
  }

setIgnoreInterfacePragmas :: DynFlags -> DynFlags
setIgnoreInterfacePragmas df = gopt_set df Opt_IgnoreInterfacePragmas

setVerbosity :: Int -> DynFlags -> DynFlags
setVerbosity n df = df { verbosity = n }

writeInterfaceFiles :: Maybe FilePath -> DynFlags -> DynFlags
writeInterfaceFiles Nothing df = df
writeInterfaceFiles (Just hi_dir) df = setHiDir hi_dir (gopt_set df Opt_WriteInterface)

setHiDir :: FilePath -> DynFlags -> DynFlags
setHiDir f d = d { hiDir      = Just f}


-- | Interpret and set the specific command line options.
-- A lot of this code is just copied from ghc/Main.hs
-- It would be good to move this code into a library module so we can just use it
-- rather than copy it.
addCmdOpts :: (GhcMonad m)
           => [String] -> DynFlags -> m (DynFlags, [G.Target])
addCmdOpts cmdOpts df1 = do
  (df2, leftovers', _warns) <- G.parseDynamicFlags df1 (map G.noLoc cmdOpts)
  -- parse targets from ghci-scripts. Only extract targets that have been ":add"'ed.
  additionalTargets <- concat <$> mapM (liftIO . getTargetsFromGhciScript) (ghciScripts df2)

  -- leftovers contains all Targets from the command line
  let leftovers = leftovers' ++ map G.noLoc additionalTargets

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
  ts <- mapM (uncurry Gap.guessTarget) srcs
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

-- | Make filepaths in the given 'DynFlags' absolute.
-- This makes the 'DynFlags' independent of the current working directory.
makeDynFlagsAbsolute :: FilePath -> DynFlags -> DynFlags
makeDynFlagsAbsolute work_dir df =
  mapOverIncludePaths makeAbs
  $ df
    { importPaths = map makeAbs (importPaths df)
    , packageDBFlags =
        map (Gap.overPkgDbRef makeAbs) (packageDBFlags df)
    }
  where
    makeAbs = (work_dir </>)

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

-- --------------------------------------------------------

-- | Read a ghci script and extract all targets to load form it.
-- The ghci script is expected to have the following format:
-- @
--  :add Foo Bar Main.hs
-- @
--
-- We strip away ":add" and parse the Targets.
getTargetsFromGhciScript :: FilePath -> IO [String]
getTargetsFromGhciScript script = do
  contents <- lines <$> readFile script
  let parseGhciLine = concatMap fst . filter (null . snd) . readP_to_S parser
  return $ concatMap parseGhciLine contents

-- |This parser aims to parse targets and double-quoted filepaths that are separated by spaces
-- and prefixed with the literal ":add"
--
-- >>> filter (null . snd) $ readP_to_S parser ":add Lib Lib2"
-- [(["Lib","Lib2"],"")]
--
-- >>> filter (null . snd) $ readP_to_S parser ":add Lib Lib2 \"Test Example.hs\""
-- [(["Lib","Lib2","Test Example.hs"],"")]
--
-- >>> filter (null . snd) $ readP_to_S parser ":add Lib Lib2 \"Test Exa\\\"mple.hs\""
-- [(["Lib","Lib2","Test Exa\"mple.hs"],"")]
parser :: ReadP [String]
parser = do
  _ <- string ":add" <* space1
  scriptword `sepBy` space1

space1 :: ReadP [Char]
space1 = many1 (char ' ')

scriptword :: ReadP String
scriptword = quoted <++ value

-- | A balanced double-quoted string
quoted :: ReadP String
quoted = do
    _ <- char '"'
    manyTill (escaped '"' <|> anyToken) $ char '"'

escaped :: Char -> ReadP Char
escaped c = c <$ string ("\\" <> [c])

value :: ReadP String
value = many1 (satisfy (not . isSpace))

anyToken :: ReadP Char
anyToken = satisfy $ const True

-- Used for clipping the trailing newlines on GHC output
-- Also only take the last line of output
-- (Stack's ghc output has a lot of preceding noise from 7zip etc)
trim :: String -> String
trim s = case lines s of
  [] -> s
  ls -> dropWhileEnd isSpace $ last ls
