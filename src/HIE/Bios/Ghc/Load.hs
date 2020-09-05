{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
-- | Convenience functions for loading a file into a GHC API session
module HIE.Bios.Ghc.Load ( loadFileWithMessage, loadFile, setTargetFiles, setTargetFilesWithMessage) where

import GHC
import qualified GHC as G
import qualified GhcMake as G
import qualified HscMain as G
import HscTypes
import Control.Monad.IO.Class

import Data.IORef

import Hooks
import TcRnTypes (FrontendResult(..))
import Control.Monad (forM, void)
import GhcMonad
import HscMain
import Data.List

import Data.Time.Clock
import qualified HIE.Bios.Ghc.Gap as Gap
import qualified HIE.Bios.Internal.Log as Log

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
         => Maybe G.Messager -- ^ Optional messager hook
                             -- to log messages produced by GHC.
         -> (FilePath, FilePath)  -- ^ Target file to load.
         -> m (Maybe TypecheckedModule, [TypecheckedModule])
         -- ^ Typechecked module and modules that had to
         -- be loaded for the target.
loadFileWithMessage msg file = do
  -- STEP 1: Load the file into the session, using collectASTs to also retrieve
  -- typechecked and parsed modules.
  (_, tcs) <- collectASTs $ (setTargetFilesWithMessage msg [file])
  Log.debugm $ "loaded " ++ fst file ++ " - " ++ snd file
  let get_fp = ml_hs_file . ms_location . pm_mod_summary . tm_parsed_module
  Log.debugm $ "Typechecked modules for: " ++ (unlines $ map (show . get_fp) tcs)
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
         => (FilePath, FilePath) -- ^ Target file to load.
         -> m (Maybe TypecheckedModule, [TypecheckedModule])
         -- ^ Typechecked module and modules that had to
         -- be loaded for the target.
loadFile = loadFileWithMessage (Just G.batchMsg)


-- | Set the files as targets and load them. This will reset GHC's targets so only the modules you
-- set as targets and its dependencies will be loaded or reloaded.
-- Produced diagnostics will be printed similar to the normal output of GHC.
-- To configure this, use 'setTargetFilesWithMessage'.
setTargetFiles :: GhcMonad m => [(FilePath, FilePath)] -> m ()
setTargetFiles = setTargetFilesWithMessage (Just G.batchMsg)

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
        | any (msTargetIs ms) ts = ms {ms_hs_date = cur_time}
        | otherwise = ms
  pure $ Gap.mapMG go graph

-- | Set the files as targets and load them. This will reset GHC's targets so only the modules you
-- set as targets and its dependencies will be loaded or reloaded.
setTargetFilesWithMessage :: (GhcMonad m)  => Maybe G.Messager -> [(FilePath, FilePath)] -> m ()
setTargetFilesWithMessage msg files = do
    targets <- forM files guessTargetMapped
    Log.debugm $ "setTargets: " ++ show files
    G.setTargets targets
    mod_graph <- updateTime targets =<< depanal [] False
    Log.debugm $ "modGraph: " ++ show (map ms_location $ Gap.mgModSummaries mod_graph)
    void $ G.load' LoadAllTargets msg mod_graph

-- | Add a hook to record the contents of any 'TypecheckedModule's which are produced
-- during compilation.
collectASTs :: (GhcMonad m) => m a -> m (a, [TypecheckedModule])
collectASTs action = do
  dflags0 <- getSessionDynFlags
  ref1 <- liftIO $ newIORef []
  let dflags1 = dflags0 { hooks = (hooks dflags0)
                          { hscFrontendHook = Just (astHook ref1) }
                        }
  -- Modify session is much faster than `setSessionDynFlags`.
  modifySession $ Gap.set_hsc_dflags dflags1
  res <- action
  tcs <- liftIO $ readIORef ref1
  -- Unset the hook so that we don't retain the reference ot the IORef so it can be gced.
  -- This stops the typechecked modules being retained in some cases.
  liftIO $ writeIORef ref1 []
  dflags_old <- getSessionDynFlags
  let dflags2 = dflags1 { hooks = (hooks dflags_old)
                          { hscFrontendHook = Nothing }
                        }
  modifySession $ Gap.set_hsc_dflags dflags2

  return (res, tcs)

-- | This hook overwrites the default frontend action of GHC.
astHook :: IORef [TypecheckedModule] -> ModSummary -> Hsc FrontendResult
astHook tc_ref ms = ghcInHsc $ do
  p <- G.parseModule =<< initializePluginsGhc ms
  tcm <- G.typecheckModule p
  let tcg_env = fst (tm_internals_ tcm)
  liftIO $ modifyIORef tc_ref (tcm :)
  return $ FrontendTypecheck tcg_env

initializePluginsGhc :: ModSummary -> Ghc ModSummary
initializePluginsGhc ms = do
  hsc_env <- getSession
  df <- liftIO $ Gap.initializePlugins hsc_env (ms_hspp_opts  ms)
  Log.debugm ("init-plugins(loaded):" ++ show (Gap.numLoadedPlugins df))
  Log.debugm ("init-plugins(specified):" ++ show (length $ pluginModNames df))
  return (ms { ms_hspp_opts = df })


ghcInHsc :: Ghc a -> Hsc a
ghcInHsc gm = do
  hsc_session <- getHscEnv
  session <- liftIO $ newIORef hsc_session
  liftIO $ reflectGhc gm (Session session)

-- | A variant of 'guessTarget' which after guessing the target for a filepath, overwrites the
-- target file to be a temporary file.
guessTargetMapped :: (GhcMonad m) => (FilePath, FilePath) -> m Target
guessTargetMapped (orig_file_name, mapped_file_name) = do
  t <- Gap.guessTarget orig_file_name Nothing
  return (setTargetFilename mapped_file_name t)

setTargetFilename :: FilePath -> Target -> Target
setTargetFilename fn t =
  t { targetId = case targetId t of
                  TargetFile _ p -> TargetFile fn p
                  tid -> tid }
