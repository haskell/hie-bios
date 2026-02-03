{-# LANGUAGE FlexibleInstances, CPP, PatternSynonyms #-}
-- | All the CPP for GHC version compability should live in this module.
module HIE.Bios.Ghc.Gap (
  ghcVersion
  -- * Warnings, Doc Compat
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
  -- * OsPath Compat
  , unsafeEncodeUtf
  , unsafeDecodeUtf
  -- * misc
  , getTyThing
  , fixInfo
  , Tc.FrontendResult(..)
  , Hsc
  , mapMG
  , mgModSummaries
  , unsetLogAction
  , load'
  , homeUnitId_
  , getDynFlags
  ) where

import Control.Monad.IO.Class
import qualified Control.Monad.Catch as E

import GHC
import qualified GHC as G

----------------------------------------------------------------
----------------------------------------------------------------

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
import qualified GHC.Driver.Make as G
import System.OsPath (OsPath)
import qualified System.OsPath as OsPath
import GHC.Unit.Types (UnitId)
import Data.Maybe (fromMaybe)
import GHC.Stack.Types (HasCallStack)

#if __GLASGOW_HASKELL__ < 904
import qualified GHC.Driver.Main as G
#endif
#if __GLASGOW_HASKELL__ >= 907
import GHC.Types.Error (mkUnknownDiagnostic, Messages)
import GHC.Driver.Errors.Types (DriverMessage)
#endif
#if __GLASGOW_HASKELL__ < 907
import GHC.Driver.CmdLine as CmdLine
#endif

ghcVersion :: String
ghcVersion = VERSION_ghc

#if __GLASGOW_HASKELL__ >= 907
load' :: GhcMonad m => Maybe G.ModIfaceCache -> LoadHowMuch -> Maybe Messager -> ModuleGraph -> m SuccessFlag
load' mhmi_cache how_much = G.load' mhmi_cache how_much mkUnknownDiagnostic
#elif __GLASGOW_HASKELL__ >= 904
load' :: GhcMonad m => Maybe G.ModIfaceCache -> LoadHowMuch -> Maybe Messager -> ModuleGraph -> m SuccessFlag
load' = G.load'
#else
load' :: GhcMonad m => a -> LoadHowMuch -> Maybe G.Messager -> ModuleGraph -> m SuccessFlag
load' _ a b c = G.load' a b c
#endif

bracket :: E.MonadMask m => m a -> (a -> m c) -> (a -> m b) -> m b
bracket =
  E.bracket

handle :: (E.MonadCatch m, E.Exception e) => (e -> m a) -> m a -> m a
handle = E.handle

catch :: (E.MonadCatch m, E.Exception e) => m a -> (e -> m a) -> m a
catch =
  E.catch

----------------------------------------------------------------

pattern RealSrcSpan :: G.RealSrcSpan -> G.SrcSpan
pattern RealSrcSpan t <- G.RealSrcSpan t _

----------------------------------------------------------------

setNoCode :: DynFlags -> DynFlags
#if __GLASGOW_HASKELL__ >= 905
setNoCode d = d { G.backend = G.noBackend }
#else
setNoCode d = d { G.backend = G.NoBackend }
#endif

----------------------------------------------------------------

set_hsc_dflags :: DynFlags -> HscEnv -> HscEnv
set_hsc_dflags dflags hsc_env = hsc_env { G.hsc_dflags = dflags }

overPkgDbRef :: (OsPath -> OsPath) -> G.PackageDBFlag -> G.PackageDBFlag
overPkgDbRef f (G.PackageDB pkgConfRef) = G.PackageDB $ case pkgConfRef of
    G.PkgDbPath fp ->
#if __GLASGOW_HASKELL__ >= 915
      G.PkgDbPath (f fp)
#else
      G.PkgDbPath (unsafeDecodeUtf $ f $ unsafeEncodeUtf fp)
#endif

    conf -> conf
overPkgDbRef _f db = db

----------------------------------------------------------------

guessTarget :: GhcMonad m => String -> Maybe UnitId -> Maybe G.Phase -> m G.Target
guessTarget = G.guessTarget

----------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 905
makeUserStyle :: DynFlags -> NamePprCtx -> PprStyle
#else
makeUserStyle :: DynFlags -> PrintUnqualified -> PprStyle
#endif
makeUserStyle _dflags style = mkUserStyle style AllTheWay

----------------------------------------------------------------

getModSummaries :: ModuleGraph -> [ModSummary]
getModSummaries = mgModSummaries

getTyThing :: (a, b, c, d, e) -> a
getTyThing (t,_,_,_,_) = t

fixInfo :: (a, b, c, d, e) -> (a, b, c, d)
fixInfo (t,f,cs,fs,_) = (t,f,cs,fs)

----------------------------------------------------------------

mapOverIncludePaths :: (FilePath -> FilePath) -> DynFlags -> DynFlags
mapOverIncludePaths f df = df
  { includePaths =
      G.IncludeSpecs
          (map f $ G.includePathsQuote  (includePaths df))
          (map f $ G.includePathsGlobal (includePaths df))
          (map f $ G.includePathsQuoteImplicit (includePaths df))
  }

----------------------------------------------------------------

unsetLogAction :: GhcMonad m => m ()
unsetLogAction = do
    hsc_env <- getSession
    logger <- liftIO $ initLogger
    let env = hsc_env { hsc_logger = pushLogHook (const noopLogger) logger }
    setSession env

noopLogger :: LogAction
#if __GLASGOW_HASKELL__ >= 903
noopLogger = (\_wr _s _ss _m -> return ())
#else
noopLogger = (\_df _wr _s _ss _m -> return ())
#endif

-- --------------------------------------------------------
-- Doc Compat functions
-- --------------------------------------------------------

pageMode :: Ppr.Mode
pageMode =
  Ppr.PageMode True

oneLineMode :: Ppr.Mode
oneLineMode = Ppr.OneLineMode

-- --------------------------------------------------------
-- DynFlags Compat functions
-- --------------------------------------------------------

numLoadedPlugins :: HscEnv -> Int
#if __GLASGOW_HASKELL__ >= 903
numLoadedPlugins = length . Plugins.pluginsWithArgs . hsc_plugins
#else
numLoadedPlugins = length . Plugins.plugins
#endif

initializePluginsForModSummary :: HscEnv -> ModSummary -> IO (Int, [G.ModuleName], ModSummary)
initializePluginsForModSummary hsc_env' mod_summary = do
  hsc_env <- DynamicLoading.initializePlugins hsc_env'
  pure ( numLoadedPlugins hsc_env
       , pluginModNames $ hsc_dflags hsc_env
       , mod_summary
       )

setFrontEndHooks :: Maybe (ModSummary -> G.Hsc Tc.FrontendResult) -> HscEnv -> HscEnv
setFrontEndHooks frontendHook env =
  env
    { hsc_hooks = hooks
        { hscFrontendHook = frontendHook
        }
    }
  where
    hooks = hsc_hooks env

getLogger :: HscEnv -> Logger
getLogger =
    hsc_logger

gopt_set :: DynFlags -> G.GeneralFlag -> DynFlags
gopt_set = G.gopt_set

setWayDynamicIfHostIsDynamic :: DynFlags -> DynFlags
setWayDynamicIfHostIsDynamic =
  if hostIsDynamic
    then
      updateWays . addWay' WayDyn
    else
      id

updateWays :: DynFlags -> DynFlags
updateWays = id

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

parseDynamicFlags :: MonadIO m
    => Logger
    -> DynFlags
    -> [G.Located String]
    -> m (DynFlags, [G.Located String]
#if __GLASGOW_HASKELL__ >= 907
          , Messages DriverMessage)
#else
          , [CmdLine.Warn])
#endif
parseDynamicFlags = G.parseDynamicFlags


parseTargetFiles :: DynFlags -> [String] -> (DynFlags, [(String, Maybe G.Phase)], [String])
parseTargetFiles = G.parseTargetFiles

-- --------------------------------------------------------
-- Platform constants
-- --------------------------------------------------------

hostIsDynamic :: Bool
hostIsDynamic = Platform.hostIsDynamic

-- --------------------------------------------------------
-- OsPath Compat
-- --------------------------------------------------------

unsafeEncodeUtf :: HasCallStack => FilePath -> OsPath
unsafeEncodeUtf fp =
#if MIN_VERSION_filepath(1,5,0)
  OsPath.unsafeEncodeUtf fp
#else
  fromMaybe (error "unsafeEncodeUtf") $ OsPath.encodeUtf fp
#endif

unsafeDecodeUtf :: HasCallStack => OsPath -> FilePath
unsafeDecodeUtf = fromMaybe (error "unsafeDecodeUtf") . OsPath.decodeUtf
