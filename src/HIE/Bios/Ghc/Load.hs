{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
-- | Convenience functions for loading a file into a GHC API session
module HIE.Bios.Ghc.Load  where


import Colog.Core (LogAction (..), WithSeverity (..), Severity (..), (<&))
import Control.Monad (forM, void)
import Control.Monad.IO.Class

import Data.List
import Data.Time.Clock
import Data.IORef

import GHC
import qualified GHC as G

#if __GLASGOW_HASKELL__ >= 900
import qualified GHC.Driver.Main as G
#else
import qualified GhcMake as G
import qualified HscMain as G
#endif

import qualified HIE.Bios.Ghc.Gap as Gap

import Prettyprinter

data Log =
  LogLoaded FilePath FilePath
  | LogTypechecked [TypecheckedModule]
  | LogInitPlugins Int [ModuleName]
  | LogSetTargets [(FilePath, FilePath)]
  | LogModGraph ModuleGraph

instance Pretty Log where
  pretty (LogLoaded fp1 fp2) = "Loaded" <+> viaShow fp1 <+> "-" <+> viaShow fp2
  pretty (LogTypechecked tcs) = "Typechecked modules for:" <+> (cat $ map (viaShow . get_fp) tcs)
  pretty (LogInitPlugins n ns) = "Loaded" <+> viaShow n <+> "plugins, specified" <+> viaShow (length ns)
  pretty (LogSetTargets ts) = "Set targets:" <+> viaShow ts
  pretty (LogModGraph mod_graph) = "ModGraph:" <+> viaShow (map ms_location $ Gap.mgModSummaries mod_graph)

get_fp :: TypecheckedModule -> Maybe FilePath
get_fp = ml_hs_file . ms_location . pm_mod_summary . tm_parsed_module

-- | Load a target into the GHC session.
--
-- The target is represented as a tuple. The tuple consists of the
-- original filename and another file that contains the actual
-- source code to compile.
--
-- The optional messager can be used to log diagnostics, warnings or errors
-- that occurred during loading the target.
--
-- If the loading succeeds, the typechecked module is returned
-- together with all the typechecked modules that had to be loaded
-- in order to typecheck the given target.
loadFileWithMessage :: GhcMonad m
         => LogAction IO (WithSeverity Log)
         -> Maybe G.Messager -- ^ Optional messager hook
                             -- to log messages produced by GHC.
         -> (FilePath, FilePath)  -- ^ Target file to load.
         -> m (Maybe TypecheckedModule, [TypecheckedModule])
         -- ^ Typechecked module and modules that had to
         -- be loaded for the target.
loadFileWithMessage logger msg file = do
  -- STEP 1: Load the file into the session, using collectASTs to also retrieve
  -- typechecked and parsed modules.
  (_, tcs) <- collectASTs logger $ (setTargetFilesWithMessage logger msg [file])
  liftIO $ logger <& LogLoaded (fst file) (snd file) `WithSeverity` Debug
  liftIO $ logger <& LogTypechecked tcs `WithSeverity` Debug
  -- Find the specific module in the list of returned typechecked modules if it exists.
  let findMod [] = Nothing
      findMod (x:xs) = case get_fp x of
                         Just fp -> if fp `isSuffixOf` (snd file) then Just x else findMod xs
                         Nothing -> findMod xs
  return (findMod tcs, tcs)

-- | Load a target into the GHC session with the default messager
--  which outputs updates in the same format as normal GHC.
--
-- The target is represented as a tuple. The tuple consists of the
-- original filename and another file that contains the actual
-- source code to compile.
--
-- If the message should configured, use 'loadFileWithMessage'.
--
-- If the loading succeeds, the typechecked module is returned
-- together with all the typechecked modules that had to be loaded
-- in order to typecheck the given target.
loadFile :: (GhcMonad m)
         => LogAction IO (WithSeverity Log)
         -> (FilePath, FilePath) -- ^ Target file to load.
         -> m (Maybe TypecheckedModule, [TypecheckedModule])
         -- ^ Typechecked module and modules that had to
         -- be loaded for the target.
loadFile logger = loadFileWithMessage logger (Just G.batchMsg)


-- | Set the files as targets and load them. This will reset GHC's targets so only the modules you
-- set as targets and its dependencies will be loaded or reloaded.
-- Produced diagnostics will be printed similar to the normal output of GHC.
-- To configure this, use 'setTargetFilesWithMessage'.
setTargetFiles
  :: GhcMonad m
  => LogAction IO (WithSeverity Log)
  -> [(FilePath, FilePath)]
  -> m ()
setTargetFiles logger = setTargetFilesWithMessage logger (Just G.batchMsg)

msTargetIs :: ModSummary -> Target -> Bool
msTargetIs ms t = case targetId t of
  TargetModule m -> moduleName (ms_mod ms) == m
  TargetFile f _ -> ml_hs_file (ms_location ms) == Just f

-- | We bump the times for any ModSummary's that are Targets, to
-- fool the recompilation checker so that we can get the typechecked modules
updateTime :: MonadIO m => [Target] -> ModuleGraph -> m ModuleGraph
updateTime ts graph = liftIO $ do
  cur_time <- getCurrentTime
  let go ms
        | any (msTargetIs ms) ts =
#if __GLASGOW_HASKELL__ >= 903
            ms {ms_hs_hash = fingerprint0}
#else
            ms {ms_hs_date = cur_time}
#endif
        | otherwise = ms
  pure $ Gap.mapMG go graph

-- | Set the files as targets and load them. This will reset GHC's targets so only the modules you
-- set as targets and its dependencies will be loaded or reloaded.
setTargetFilesWithMessage
  :: (GhcMonad m)
  => LogAction IO (WithSeverity Log)
  -> Maybe G.Messager
  -> [(FilePath, FilePath)]
  -> m ()
setTargetFilesWithMessage logger msg files = do
    targets <- forM files guessTargetMapped
    liftIO $ logger <& LogSetTargets files `WithSeverity` Debug
    G.setTargets targets
    mod_graph <- updateTime targets =<< depanal [] False
    liftIO $ logger <& LogModGraph mod_graph `WithSeverity` Debug
    void $ Gap.load' Nothing LoadAllTargets msg mod_graph

-- | Add a hook to record the contents of any 'TypecheckedModule's which are produced
-- during compilation.
collectASTs
  :: (GhcMonad m)
  => LogAction IO (WithSeverity Log)
  -> m a
  -> m (a, [TypecheckedModule])
collectASTs logger action = do
  ref1 <- liftIO $ newIORef []
  -- Modify session is much faster than `setSessionDynFlags`.
  Gap.modifySession $ Gap.setFrontEndHooks (Just (astHook logger ref1))
  res <- action
  tcs <- liftIO $ readIORef ref1
  -- Unset the hook so that we don't retain the reference to the IORef so it can be GCed.
  -- This stops the typechecked modules being retained in some cases.
  liftIO $ writeIORef ref1 []
  Gap.modifySession $ Gap.setFrontEndHooks Nothing

  return (res, tcs)

-- | This hook overwrites the default frontend action of GHC.
astHook
  :: LogAction IO (WithSeverity Log)
  -> IORef [TypecheckedModule]
  -> ModSummary
  -> Gap.Hsc Gap.FrontendResult
astHook logger tc_ref ms = ghcInHsc $ do
  p <- G.parseModule =<< initializePluginsGhc logger ms
  tcm <- G.typecheckModule p
  let tcg_env = fst (tm_internals_ tcm)
  liftIO $ modifyIORef tc_ref (tcm :)
  return $ Gap.FrontendTypecheck tcg_env

initializePluginsGhc
  :: GhcMonad m
  => LogAction IO (WithSeverity Log)
  -> ModSummary
  -> m ModSummary
initializePluginsGhc logger ms = do
  hsc_env <- getSession
  (pluginsLoaded, pluginNames, newMs) <- liftIO $ Gap.initializePluginsForModSummary hsc_env ms
  liftIO $ logger <& LogInitPlugins pluginsLoaded pluginNames `WithSeverity` Debug
  return newMs

ghcInHsc :: Ghc a -> Gap.Hsc a
ghcInHsc gm = do
  hsc_session <- Gap.getHscEnv
  session <- liftIO $ newIORef hsc_session
  liftIO $ Gap.reflectGhc gm (Gap.Session session)

-- | A variant of 'guessTarget' which after guessing the target for a filepath, overwrites the
-- target file to be a temporary file.
guessTargetMapped :: (GhcMonad m) => (FilePath, FilePath) -> m Target
guessTargetMapped (orig_file_name, mapped_file_name) = do
  df <- Gap.getDynFlags
  t <- Gap.guessTarget orig_file_name (Just $ Gap.homeUnitId_ df) Nothing
  return (setTargetFilename mapped_file_name t)

setTargetFilename :: FilePath -> Target -> Target
setTargetFilename fn t =
  t { targetId = case targetId t of
                  TargetFile _ p -> TargetFile fn p
                  tid -> tid }
