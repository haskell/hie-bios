{-# LANGUAGE FlexibleInstances, CPP, PatternSynonyms #-}
-- | All the CPP for GHC version compability should live in this module.
module HIE.Bios.Ghc.Gap (
  ghcVersion
  -- * Warnings, Doc Compat
  , WarnFlags
  , emptyWarnFlags
  , makeUserStyle
  , PprStyle
  -- * Argument parsing
  , HIE.Bios.Ghc.Gap.parseTargetFiles
  -- * Ghc Monad
  , G.modifySession
  , G.reflectGhc
  , G.Session(..)
  -- * Hsc Monad
  , getHscEnv
  -- * Driver compat
  , batchMsg
  -- * HscEnv Compat
  , set_hsc_dflags
  , overPkgDbRef
  , HIE.Bios.Ghc.Gap.guessTarget
  , setNoCode
  , getModSummaries
  , mapOverIncludePaths
  , HIE.Bios.Ghc.Gap.getLogger
  -- * AST compat
  , pattern HIE.Bios.Ghc.Gap.RealSrcSpan
  , LExpression
  , LBinding
  , LPattern
  , inTypes
  , outType
  -- * Exceptions
  , catch
  , bracket
  , handle
  -- * Doc Gap functions
  , pageMode
  , oneLineMode
  -- * DynFlags compat
  , initializePluginsForModSummary
  , setFrontEndHooks
  , updOptLevel
  , setWayDynamicIfHostIsDynamic
  , HIE.Bios.Ghc.Gap.gopt_set
  , HIE.Bios.Ghc.Gap.parseDynamicFlags
  -- * Platform constants
  , hostIsDynamic
  -- * misc
  , getModuleName
  , getTyThing
  , fixInfo
  , Tc.FrontendResult(..)
  , Hsc
  , mapMG
  , mgModSummaries
  , unsetLogAction
  ) where

import Control.Monad.IO.Class
import qualified Control.Monad.Catch as E

import GHC
import qualified GHC as G

#if __GLASGOW_HASKELL__ >= 804 && __GLASGOW_HASKELL__ < 900
import Data.List
import System.FilePath

import DynFlags (LogAction, WarningFlag, updOptLevel, Way(WayDyn), updateWays, addWay')
import qualified DynFlags as G
import qualified Exception as G

import Outputable (PprStyle, Depth(AllTheWay), mkUserStyle)
import HscMain (getHscEnv, batchMsg)
import HscTypes (Hsc, HscEnv(..))
import qualified HscTypes as G
import qualified EnumSet as E (EnumSet, empty)
import qualified Pretty as Ppr
import qualified TcRnTypes as Tc
import Hooks (Hooks(hscFrontendHook))
import qualified CmdLineParser as CmdLine
import DriverPhases as G
import Util as G
import qualified GhcMonad as G

#if __GLASGOW_HASKELL__ >= 808
import qualified DynamicLoading (initializePlugins)
import qualified Plugins (plugins)
#endif

#if __GLASGOW_HASKELL__ >= 806 && __GLASGOW_HASKELL__ < 810
import HsExtension (GhcTc)
import HsExpr (MatchGroup, MatchGroupTc(..))
#elif __GLASGOW_HASKELL__ >= 804 && __GLASGOW_HASKELL__ < 810
import HsExtension (GhcTc)
import HsExpr (MatchGroup)
#endif
#endif
----------------------------------------------------------------
----------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 902
import GHC.Core.Multiplicity (irrelevantMult)
import GHC.Data.EnumSet as E
import GHC.Driver.CmdLine as CmdLine
import GHC.Driver.Env as G
import GHC.Driver.Session as G
import GHC.Driver.Hooks
import GHC.Driver.Main
import GHC.Driver.Monad as G
import qualified GHC.Driver.Plugins as Plugins
import GHC.Platform.Ways (Way(WayDyn))
import qualified GHC.Platform.Ways as Platform
import qualified GHC.Runtime.Loader as DynamicLoading (initializePlugins)
import qualified GHC.Tc.Types as Tc

import GHC.Utils.Logger
import GHC.Utils.Outputable
import qualified GHC.Utils.Ppr as Ppr
#elif __GLASGOW_HASKELL__ >= 900
import Data.List
import System.FilePath

import GHC.Core.Multiplicity (irrelevantMult)
import GHC.Data.EnumSet as E
import GHC.Driver.CmdLine as CmdLine
import GHC.Driver.Types as G
import GHC.Driver.Session as G
import GHC.Driver.Hooks
import GHC.Driver.Main
import GHC.Driver.Monad as G
import GHC.Driver.Phases as G
import GHC.Utils.Misc as G
import qualified GHC.Driver.Plugins as Plugins
import GHC.Driver.Ways (Way(WayDyn))
import qualified GHC.Driver.Ways as Platform
import qualified GHC.Runtime.Loader as DynamicLoading (initializePlugins)
import qualified GHC.Tc.Types as Tc

import GHC.Utils.Outputable
import qualified GHC.Utils.Ppr as Ppr
#endif

ghcVersion :: String
ghcVersion = VERSION_ghc

#if __GLASGOW_HASKELL__ >= 900
bracket :: E.MonadMask m => m a -> (a -> m c) -> (a -> m b) -> m b
bracket =
  E.bracket
#else
bracket :: G.ExceptionMonad m => m a -> (a -> m c) -> (a -> m b) -> m b
bracket =
  G.gbracket
#endif

#if __GLASGOW_HASKELL__ >= 900
handle :: (E.MonadCatch m, E.Exception e) => (e -> m a) -> m a -> m a
handle = E.handle
#else
handle :: (G.ExceptionMonad m, E.Exception e) => (e -> m a) -> m a -> m a
handle = G.ghandle
#endif

#if __GLASGOW_HASKELL__ >= 810
catch :: (E.MonadCatch m, E.Exception e) => m a -> (e -> m a) -> m a
catch =
  E.catch
#else
catch :: (G.ExceptionMonad m, E.Exception e) => m a -> (e -> m a) -> m a
catch =
  G.gcatch
#endif

----------------------------------------------------------------

pattern RealSrcSpan :: G.RealSrcSpan -> G.SrcSpan
#if __GLASGOW_HASKELL__ >= 900
pattern RealSrcSpan t <- G.RealSrcSpan t _
#else
pattern RealSrcSpan t <- G.RealSrcSpan t
#endif

----------------------------------------------------------------

setNoCode :: DynFlags -> DynFlags
#if __GLASGOW_HASKELL__ >= 901
setNoCode d = d { G.backend = G.NoBackend }
#else
setNoCode d = d { G.hscTarget = G.HscNothing }
#endif

----------------------------------------------------------------

set_hsc_dflags :: DynFlags -> HscEnv -> HscEnv
set_hsc_dflags dflags hsc_env = hsc_env { G.hsc_dflags = dflags }

overPkgDbRef :: (FilePath -> FilePath) -> G.PackageDBFlag -> G.PackageDBFlag
overPkgDbRef f (G.PackageDB pkgConfRef) = G.PackageDB
              $ case pkgConfRef of
#if __GLASGOW_HASKELL__ >= 900
                G.PkgDbPath fp -> G.PkgDbPath (f fp)
#else
                G.PkgConfFile fp -> G.PkgConfFile (f fp)
#endif
                conf -> conf
overPkgDbRef _f db = db

----------------------------------------------------------------

guessTarget :: GhcMonad m => String -> Maybe G.Phase -> m G.Target
#if __GLASGOW_HASKELL__ >= 901
guessTarget a b = G.guessTarget a b
#else
guessTarget a b = G.guessTarget a b
#endif

----------------------------------------------------------------

makeUserStyle :: DynFlags -> PrintUnqualified -> PprStyle
#if __GLASGOW_HASKELL__ >= 900
makeUserStyle _dflags style = mkUserStyle style AllTheWay
#elif __GLASGOW_HASKELL__ >= 804
makeUserStyle dflags style = mkUserStyle dflags style AllTheWay
#endif

#if __GLASGOW_HASKELL__ >= 804
getModuleName :: (a, b) -> a
getModuleName = fst
#endif

----------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 804
type WarnFlags = E.EnumSet WarningFlag
emptyWarnFlags :: WarnFlags
emptyWarnFlags = E.empty
#endif

#if __GLASGOW_HASKELL__ >= 804
getModSummaries :: ModuleGraph -> [ModSummary]
getModSummaries = mgModSummaries

getTyThing :: (a, b, c, d, e) -> a
getTyThing (t,_,_,_,_) = t

fixInfo :: (a, b, c, d, e) -> (a, b, c, d)
fixInfo (t,f,cs,fs,_) = (t,f,cs,fs)
#endif

----------------------------------------------------------------

mapOverIncludePaths :: (FilePath -> FilePath) -> DynFlags -> DynFlags
mapOverIncludePaths f df = df
  { includePaths =
#if __GLASGOW_HASKELL__ > 804
      G.IncludeSpecs
          (map f $ G.includePathsQuote  (includePaths df))
          (map f $ G.includePathsGlobal (includePaths df))
#else
      map f (includePaths df)
#endif
  }

----------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 806
type LExpression = LHsExpr GhcTc
type LBinding    = LHsBind GhcTc
type LPattern    = LPat    GhcTc

inTypes :: MatchGroup GhcTc LExpression -> [Type]
#if __GLASGOW_HASKELL__ >= 900
inTypes = map irrelevantMult . mg_arg_tys . mg_ext
#else
inTypes = mg_arg_tys . mg_ext
#endif
outType :: MatchGroup GhcTc LExpression -> Type
outType = mg_res_ty . mg_ext
#elif __GLASGOW_HASKELL__ >= 804
type LExpression = LHsExpr GhcTc
type LBinding    = LHsBind GhcTc
type LPattern    = LPat    GhcTc

inTypes :: MatchGroup GhcTc LExpression -> [Type]
inTypes = mg_arg_tys
outType :: MatchGroup GhcTc LExpression -> Type
outType = mg_res_ty
#endif

unsetLogAction :: GhcMonad m => m ()
unsetLogAction = do
#if __GLASGOW_HASKELL__ >= 902
    hsc_env <- getSession
    logger <- liftIO $ initLogger
    let env = hsc_env { hsc_logger = pushLogHook (const noopLogger) logger }
    setSession env
#else
    setLogAction noopLogger
#if __GLASGOW_HASKELL__ < 806
        (\_df -> return ())
#endif
#endif

noopLogger :: LogAction
#if __GLASGOW_HASKELL__ >= 900
noopLogger = (\_df _wr _s _ss _m -> return ())
#else
noopLogger = (\_df _wr _s _ss _pp _m -> return ())
#endif

-- --------------------------------------------------------
-- Doc Compat functions
-- --------------------------------------------------------

pageMode :: Ppr.Mode
pageMode =
#if __GLASGOW_HASKELL__ >= 902
  Ppr.PageMode True
#else
  Ppr.PageMode
#endif

oneLineMode :: Ppr.Mode
oneLineMode = Ppr.OneLineMode

-- --------------------------------------------------------
-- DynFlags Compat functions
-- --------------------------------------------------------

numLoadedPlugins :: HscEnv -> Int
#if __GLASGOW_HASKELL__ >= 902
numLoadedPlugins = length . Plugins.plugins
#elif __GLASGOW_HASKELL__ >= 808
numLoadedPlugins = length . Plugins.plugins . hsc_dflags
#else
-- Plugins are loaded just as they are used
numLoadedPlugins _ = 0
#endif

initializePluginsForModSummary :: HscEnv -> ModSummary -> IO (Int, [G.ModuleName], ModSummary)
initializePluginsForModSummary hsc_env' mod_summary = do
#if __GLASGOW_HASKELL__ >= 902
  hsc_env <- DynamicLoading.initializePlugins hsc_env'
  pure ( numLoadedPlugins hsc_env
       , pluginModNames $ hsc_dflags hsc_env
       , mod_summary
       )
#elif __GLASGOW_HASKELL__ >= 808
  let dynFlags' = G.ms_hspp_opts mod_summary
  dynFlags <- DynamicLoading.initializePlugins hsc_env' dynFlags'
  pure ( numLoadedPlugins $ set_hsc_dflags dynFlags hsc_env'
       , G.pluginModNames dynFlags
       , mod_summary { G.ms_hspp_opts = dynFlags }
       )
#else
  -- In earlier versions of GHC plugins are just loaded before they are used.
  return (numLoadedPlugins hsc_env', G.pluginModNames $ hsc_dflags hsc_env', mod_summary)
#endif


setFrontEndHooks :: Maybe (ModSummary -> G.Hsc Tc.FrontendResult) -> HscEnv -> HscEnv
setFrontEndHooks frontendHook env =
#if __GLASGOW_HASKELL__ >= 902
  env
    { hsc_hooks = hooks
        { hscFrontendHook = frontendHook
        }
    }
  where
    hooks = hsc_hooks env
#else
  env
    { G.hsc_dflags = flags
        { G.hooks = oldhooks
            { hscFrontendHook = frontendHook
            }
        }
    }
  where
    flags = hsc_dflags env
    oldhooks = G.hooks flags
#endif

#if __GLASGOW_HASKELL__ < 902
type Logger = ()
#endif

getLogger :: HscEnv -> Logger
getLogger =
#if __GLASGOW_HASKELL__ >= 902
    hsc_logger
#else
    const ()
#endif

gopt_set :: DynFlags -> G.GeneralFlag -> DynFlags
gopt_set = G.gopt_set

setWayDynamicIfHostIsDynamic :: DynFlags -> DynFlags
setWayDynamicIfHostIsDynamic =
  if hostIsDynamic
    then
      updateWays . addWay' WayDyn
    else
      id

#if __GLASGOW_HASKELL__ >= 900
updateWays :: DynFlags -> DynFlags
updateWays = id

#if __GLASGOW_HASKELL__ >= 902
-- Copied from GHC, do we need that?
addWay' :: Way -> DynFlags -> DynFlags
addWay' w dflags0 =
   let platform = targetPlatform dflags0
       dflags1 = dflags0 { targetWays_ = Platform.addWay w (targetWays_ dflags0) }
       dflags2 = foldr setGeneralFlag' dflags1
                       (Platform.wayGeneralFlags platform w)
       dflags3 = foldr unSetGeneralFlag' dflags2
                       (Platform.wayUnsetGeneralFlags platform w)
   in dflags3
#endif
#endif

parseDynamicFlags :: MonadIO m
    => Logger
    -> DynFlags
    -> [G.Located String]
    -> m (DynFlags, [G.Located String], [CmdLine.Warn])
#if __GLASGOW_HASKELL__ >= 902
parseDynamicFlags = G.parseDynamicFlags
#else
parseDynamicFlags _ = G.parseDynamicFlags
#endif

parseTargetFiles :: DynFlags -> [String] -> (DynFlags, [(String, Maybe G.Phase)], [String])
#if __GLASGOW_HASKELL__ >= 902
parseTargetFiles = G.parseTargetFiles
#else
parseTargetFiles dflags0 fileish_args =
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
    normal_fileish_paths = map normalise_hyp fileish_args

    (srcs, objs) = partition_args normal_fileish_paths [] []
    df1 = dflags0 { G.ldInputs = map (G.FileOption "") objs ++ G.ldInputs dflags0 }
  in
    (df1, srcs, objs)
#endif


#if __GLASGOW_HASKELL__ < 902
partition_args :: [String] -> [(String, Maybe G.Phase)] -> [String]
               -> ([(String, Maybe G.Phase)], [String])
-- partition_args, along with some of the other code in this file,
-- was copied from ghc/Main.hs
-- -----------------------------------------------------------------------------
-- Splitting arguments into source files and object files.  This is where we
-- interpret the -x <suffix> option, and attach a (Maybe Phase) to each source
-- file indicating the phase specified by the -x option in force, if any.
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
#endif

-- --------------------------------------------------------
-- Platform constants
-- --------------------------------------------------------

hostIsDynamic :: Bool
#if __GLASGOW_HASKELL__ >= 900
hostIsDynamic = Platform.hostIsDynamic
#else
hostIsDynamic = G.dynamicGhc
#endif