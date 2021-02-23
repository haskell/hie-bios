{-# LANGUAGE FlexibleInstances, CPP, PatternSynonyms #-}
-- | All the CPP for GHC version compability should live in this module.
module HIE.Bios.Ghc.Gap (
    WarnFlags
  , emptyWarnFlags
  , makeUserStyle
  , getModuleName
  , getTyThing
  , fixInfo
  , guessTarget
  , setNoCode
  , overPkgDbRef
  , set_hsc_dflags
  , getModSummaries
  , mapOverIncludePaths
  , pattern RealSrcSpan
  , LExpression
  , LBinding
  , LPattern
  , inTypes
  , outType
  , catch
  , bracket
  , handle
  , mapMG
  , mgModSummaries
  , numLoadedPlugins
  , initializePlugins
  , unsetLogAction
  ) where

import DynFlags (DynFlags, includePaths, LogAction)
import qualified DynFlags as G
import GHC(LHsBind, LHsExpr, LPat, Type, ModSummary, ModuleGraph, HscEnv, setLogAction, GhcMonad)
import qualified GHC as G
import qualified Exception as G
import Outputable (PrintUnqualified, PprStyle, Depth(AllTheWay), mkUserStyle)
import qualified HscTypes as G

import qualified Control.Monad.Catch as E

----------------------------------------------------------------
----------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 804
import DynFlags (WarningFlag)
import qualified EnumSet as E (EnumSet, empty)
import GHC (mgModSummaries, mapMG)
#endif

#if __GLASGOW_HASKELL__ >= 806
import DynFlags (IncludeSpecs(..))
#endif

#if __GLASGOW_HASKELL__ >= 808
import qualified DynamicLoading (initializePlugins)
import qualified Plugins (plugins)
#endif

#if __GLASGOW_HASKELL__ >= 810
import GHC.Hs.Extension (GhcTc)
import GHC.Hs.Expr (MatchGroup, MatchGroupTc(..), mg_ext)
#elif __GLASGOW_HASKELL__ >= 806
import HsExtension (GhcTc)
import HsExpr (MatchGroup, MatchGroupTc(..))
import GHC (mg_ext)
#elif __GLASGOW_HASKELL__ >= 804
import HsExtension (GhcTc)
import HsExpr (MatchGroup)
import GHC (mg_res_ty, mg_arg_tys)
#else
import HsExtension (GhcTc)
import HsExpr (MatchGroup)
#endif

#if __GLASGOW_HASKELL__ >= 900
import GHC.Core.Multiplicity (irrelevantMult)
#endif

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
guessTarget a b = G.guessTarget a Nothing b
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
      IncludeSpecs
          (map f $ includePathsQuote  (includePaths df))
          (map f $ includePathsGlobal (includePaths df))
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

numLoadedPlugins :: DynFlags -> Int
#if __GLASGOW_HASKELL__ >= 808
numLoadedPlugins = length . Plugins.plugins
#else
-- Plugins are loaded just as they are used
numLoadedPlugins _ = 0
#endif

initializePlugins :: HscEnv -> DynFlags -> IO DynFlags
#if __GLASGOW_HASKELL__ >= 808
initializePlugins = DynamicLoading.initializePlugins
#else
-- In earlier versions of GHC plugins are just loaded before they are used.
initializePlugins _ df = return df
#endif

unsetLogAction :: GhcMonad m => m ()
unsetLogAction =
    setLogAction noopLogger
#if __GLASGOW_HASKELL__ < 806
        (\_df -> return ())
#endif

noopLogger :: LogAction
#if __GLASGOW_HASKELL__ >= 900
noopLogger = (\_df _wr _s _ss _m -> return ())
#else
noopLogger = (\_df _wr _s _ss _pp _m -> return ())
#endif