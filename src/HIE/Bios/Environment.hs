{-# LANGUAGE RecordWildCards, CPP #-}
module HIE.Bios.Environment (initSession, getRuntimeGhcLibDir, getRuntimeGhcVersion, makeDynFlagsAbsolute, makeTargetsAbsolute, getCacheDir, addCmdOpts) where

import GHC (GhcMonad)
import qualified GHC as G

import Control.Applicative
import Control.Monad (void)
import Control.Monad.IO.Class

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
import qualified HIE.Bios.Ghc.Gap as Gap

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
        $ Gap.setWayDynamicIfHostIsDynamic     -- Add dynamic way if GHC is built with dynamic linking
        $ setLinkerOptions df''                -- Set `-fno-code` to avoid generating object files, unless we have to.
        )

    let targets' = makeTargetsAbsolute componentRoot targets
    -- Unset the default log action to avoid output going to stdout.
    Gap.unsetLogAction
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
setLinkerOptions :: G.DynFlags -> G.DynFlags
setLinkerOptions df = Gap.setNoCode $ df {
    G.ghcLink = G.LinkInMemory
  , G.ghcMode = G.CompManager
  }

setIgnoreInterfacePragmas :: G.DynFlags -> G.DynFlags
setIgnoreInterfacePragmas df = Gap.gopt_set df G.Opt_IgnoreInterfacePragmas

setVerbosity :: Int -> G.DynFlags -> G.DynFlags
setVerbosity n df = df { G.verbosity = n }

writeInterfaceFiles :: Maybe FilePath -> G.DynFlags -> G.DynFlags
writeInterfaceFiles Nothing df = df
writeInterfaceFiles (Just hi_dir) df = setHiDir hi_dir (Gap.gopt_set df G.Opt_WriteInterface)

setHiDir :: FilePath -> G.DynFlags -> G.DynFlags
setHiDir f d = d { G.hiDir      = Just f}


-- | Interpret and set the specific command line options.
-- A lot of this code is just copied from ghc/Main.hs
-- It would be good to move this code into a library module so we can just use it
-- rather than copy it.
addCmdOpts :: (GhcMonad m)
           => [String] -> G.DynFlags -> m (G.DynFlags, [G.Target])
addCmdOpts cmdOpts df1 = do
  logger <- Gap.getLogger <$> G.getSession
  (df2, leftovers', _warns) <- Gap.parseDynamicFlags logger df1 (map G.noLoc cmdOpts)
  -- parse targets from ghci-scripts. Only extract targets that have been ":add"'ed.
  additionalTargets <- concat <$> mapM (liftIO . getTargetsFromGhciScript) (G.ghciScripts df2)

  -- leftovers contains all Targets from the command line
  let leftovers = map G.unLoc leftovers' ++ additionalTargets

  let (df3, srcs, _objs) = Gap.parseTargetFiles df2 leftovers
  ts <- mapM (uncurry Gap.guessTarget) srcs
  return (df3, ts)

-- | Make filepaths in the given 'DynFlags' absolute.
-- This makes the 'DynFlags' independent of the current working directory.
makeDynFlagsAbsolute :: FilePath -> G.DynFlags -> G.DynFlags
makeDynFlagsAbsolute work_dir df =
  Gap.mapOverIncludePaths makeAbs
  $ df
    { G.importPaths = map makeAbs (G.importPaths df)
    , G.packageDBFlags =
        map (Gap.overPkgDbRef makeAbs) (G.packageDBFlags df)
    }
  where
    makeAbs = (work_dir </>)

-- --------------------------------------------------------

disableOptimisation :: G.DynFlags -> G.DynFlags
disableOptimisation df = Gap.updOptLevel 0 df

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
