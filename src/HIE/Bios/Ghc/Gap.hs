{-# LANGUAGE FlexibleInstances, CPP #-}
-- | All the CPP for GHC version compability should live in this module.
module HIE.Bios.Ghc.Gap (
    WarnFlags
  , emptyWarnFlags
  , makeUserStyle
  , getModuleName
  , getTyThing
  , fixInfo
  , getModSummaries
  , mapOverIncludePaths
  , LExpression
  , LBinding
  , LPattern
  , inTypes
  , outType
  , mapMG
  , mgModSummaries
  , numLoadedPlugins
  , initializePlugins
  , unsetLogAction
  ) where

import DynFlags (DynFlags, includePaths)
import GHC(LHsBind, LHsExpr, LPat, Type, ModSummary, ModuleGraph, HscEnv, setLogAction, GhcMonad)
import Outputable (PrintUnqualified, PprStyle, Depth(AllTheWay), mkUserStyle)

#if __GLASGOW_HASKELL__ >= 808
import qualified DynamicLoading (initializePlugins)
import qualified Plugins (plugins)
#endif





----------------------------------------------------------------
----------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 804
import DynFlags (WarningFlag)
import qualified EnumSet as E (EnumSet, empty)
import GHC (mgModSummaries, mapMG)
#endif

#if __GLASGOW_HASKELL__ >= 810
import GHC.Hs.Extension (GhcTc)
import GHC.Hs.Expr (MatchGroup, MatchGroupTc(..), mg_ext)
#elif __GLASGOW_HASKELL__ >= 806
import DynFlags (IncludeSpecs(..))
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

----------------------------------------------------------------
----------------------------------------------------------------

makeUserStyle :: DynFlags -> PrintUnqualified -> PprStyle
#if __GLASGOW_HASKELL__ >= 804
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
inTypes = mg_arg_tys . mg_ext
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
    setLogAction (\_df _wr _s _ss _pp _m -> return ())
#if __GLASGOW_HASKELL__ < 806
        (\_df -> return ())
#endif