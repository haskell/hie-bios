{-# LANGUAGE FlexibleInstances, CPP, PatternSynonyms #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE TupleSections #-}
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
  -- * Compat shims
  , typecheckModule
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
  , initMulti
  , InRTS(..)
  , removeRTS
  ) where

import Control.Monad.IO.Class
import qualified Control.Monad.Catch as E

import GHC hiding (typecheckModule,parseTargetFiles)
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
import GHC.Unit.Types (UnitId)
import GHC.Utils.Logger
import GHC.Utils.Outputable
import qualified GHC.Utils.Ppr as Ppr
import qualified GHC.Driver.Make as G
import System.OsPath (OsPath)
import qualified System.OsPath as OsPath
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
#if __GLASGOW_HASKELL__ >= 914
import qualified GHC.Driver.Session.Units as G
#else
import GHC.Unit.Home.ModInfo (emptyHomePackageTable)
import GHC.Unit.Env
import Control.Monad
import GHC.ResponseFile (expandResponse)
import Data.List (partition)
import GHC.Driver.Phases (isHaskellishTarget)
import qualified GHC.Unit.State as State
import System.FilePath (isRelative, (</>))
import qualified Data.Map as Map
#endif
import qualified Data.List.NonEmpty as NE
import Data.List (isPrefixOf, isSuffixOf)

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

typecheckModule :: GhcMonad m => ParsedModule -> m TypecheckedModule
#if MIN_VERSION_ghc(10, 1, 0)
typecheckModule = G.typecheckModule G.StartAndStopTcMPlugins
#else
typecheckModule = G.typecheckModule
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
guessTarget a b c = G.guessTarget a b c

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
noopLogger = (\_wr _s _ss _m -> return ())

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
numLoadedPlugins = length . Plugins.pluginsWithArgs . hsc_plugins

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

initMulti :: NE.NonEmpty String -> (DynFlags     -> [(String, Maybe Phase)] -> [String] -> [String] -> IO ()) -> Ghc [(String, Maybe UnitId, Maybe Phase)]
#if __GLASGOW_HASKELL__ >= 914
initMulti = G.initMulti
#else

-- taken from ghc-9.6.7, dropping some checks and logging.
initMulti unitArgsFiles _check = do
  hsc_env <- GHC.getSession
  let logger = hsc_logger hsc_env
  initial_dflags <- GHC.getSessionDynFlags

  dynFlagsAndSrcs <- forM unitArgsFiles $ \f -> do
    when (verbosity initial_dflags > 2) (liftIO $ print f)
    args <- liftIO $ expandResponse [f]
    (dflags2, fileish_args, _warns) <- parseDynamicFlagsCmdLine initial_dflags (map (mkGeneralLocated f) (removeRTS args))

    let (dflags3, srcs, _objs) = parseTargetFiles dflags2 (map unLoc fileish_args)
        dflags4 = offsetDynFlags dflags3

    let (hs_srcs, _non_hs_srcs) = partition isHaskellishTarget srcs
    pure (dflags4, hs_srcs)

  let
    unitDflags = NE.map fst dynFlagsAndSrcs
    srcs = NE.map (\(dflags, lsrcs) -> map (uncurry (,Just $ homeUnitId_ dflags,)) lsrcs) dynFlagsAndSrcs
    (hs_srcs, _non_hs_srcs) = unzip (map (partition (\(file, _uid, phase) -> isHaskellishTarget (file, phase))) (NE.toList srcs))

  let (initial_home_graph, mainUnitId) = createUnitEnvFromFlags unitDflags
      home_units = unitEnv_keys initial_home_graph

  home_unit_graph <- forM initial_home_graph $ \homeUnitEnv -> do
    let cached_unit_dbs = homeUnitEnv_unit_dbs homeUnitEnv
        hue_flags = homeUnitEnv_dflags homeUnitEnv
        dflags = homeUnitEnv_dflags homeUnitEnv
    (dbs,unit_state,home_unit,mconstants) <- liftIO $ State.initUnits logger hue_flags cached_unit_dbs home_units

    updated_dflags <- liftIO $ updatePlatformConstants dflags mconstants
    pure $ HomeUnitEnv
      { homeUnitEnv_units = unit_state
      , homeUnitEnv_unit_dbs = Just dbs
      , homeUnitEnv_dflags = updated_dflags
      , homeUnitEnv_hpt = emptyHomePackageTable
      , homeUnitEnv_home_unit = Just home_unit
      }

  let dflags = homeUnitEnv_dflags $ unitEnv_lookup mainUnitId home_unit_graph
  unitEnv <- assertUnitEnvInvariant <$> (liftIO $ initUnitEnv mainUnitId home_unit_graph (ghcNameVersion dflags) (targetPlatform dflags))
  let final_hsc_env = hsc_env { hsc_unit_env = unitEnv }

  GHC.setSession final_hsc_env

  return $ concat hs_srcs

offsetDynFlags :: DynFlags -> DynFlags
offsetDynFlags dflags =
  dflags { hiDir = c hiDir
         , objectDir  = c objectDir
         , stubDir = c stubDir
         , hieDir  = c hieDir
         , dumpDir = c dumpDir  }

  where
    c f = augment_maybe (f dflags)

    augment_maybe Nothing = Nothing
    augment_maybe (Just f) = Just (augment f)
    augment f | isRelative f, Just offset <- workingDirectory dflags = offset </> f
              | otherwise = f


createUnitEnvFromFlags :: NE.NonEmpty DynFlags -> (HomeUnitGraph, UnitId)
createUnitEnvFromFlags unitDflags =
  let
    newInternalUnitEnv dflags = mkHomeUnitEnv dflags emptyHomePackageTable Nothing
    unitEnvList = NE.map (\dflags -> (homeUnitId_ dflags, newInternalUnitEnv dflags)) unitDflags
    activeUnit = fst $ NE.head unitEnvList
  in
    (unitEnv_new (Map.fromList (NE.toList (unitEnvList))), activeUnit)


#endif

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
