{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module HIE.Bios.Ghc.Load ( loadFileWithMessage
                         , loadFile
                         , setTargetFiles
                         , setTargetFilesWithMessage
                         , reloadFile) where

import CoreMonad (liftIO)
import GHC
import qualified GHC as G
import qualified GhcMake as G
import qualified HscMain as G
import Outputable (showSDocUnsafe, ppr)
import HscTypes
import Control.Monad.IO.Class

import Data.IORef

import System.Directory
import Hooks
import TcRnTypes (FrontendResult(..))
import Control.Monad (forM, void)
import GhcMonad
import HscMain
import Data.List
import Debug.Trace

import Data.Time.Clock
import qualified HIE.Bios.Ghc.Gap as Gap
import qualified HIE.Bios.Log as Log

-- | Obtaining type of a target expression. (GHCi's type:)
loadFileWithMessage :: GhcMonad m
         => Maybe G.Messager
         -> (FilePath, FilePath)     -- ^ A target file.
         -> m (Maybe TypecheckedModule, [TypecheckedModule])
loadFileWithMessage msg file = do
  dir <- liftIO $ getCurrentDirectory
  Log.debugm $ "loadFile:2 " ++ dir
  df <- getSessionDynFlags
  Log.debugm $ "loadFile:3 " ++ show (optLevel df)
  (_, tcs) <- collectASTs $
    (setTargetFilesWithMessage msg [file])
  Log.debugm $ "loaded " ++ fst file ++ " - " ++ snd file
  let get_fp = ml_hs_file . ms_location . pm_mod_summary . tm_parsed_module
  Log.debugm $ "Typechecked modules for: " ++ (unlines $ map (show . get_fp) tcs)
  let findMod [] = Nothing
      findMod (x:xs) = case get_fp x of
                         Just fp -> if fp `isSuffixOf` (snd file) then Just x else findMod xs
                         Nothing -> findMod xs
  return (findMod tcs, tcs)

loadFile :: (GhcMonad m)
         => (FilePath, FilePath)
         -> m (Maybe TypecheckedModule, [TypecheckedModule])
loadFile = loadFileWithMessage (Just G.batchMsg)

{-
fileModSummary :: GhcMonad m => FilePath -> m ModSummary
fileModSummary file = do
    mss <- getModSummaries <$> G.getModuleGraph
    let [ms] = filter (\m -> G.ml_hs_file (G.ms_location m) == Just file) mss
    return ms
    -}


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

-- | Set the files as targets and load them.
setTargetFilesWithMessage :: (GhcMonad m)  => Maybe G.Messager -> [(FilePath, FilePath)] -> m ()
setTargetFilesWithMessage msg files = do
    targets <- forM files guessTargetMapped
    Log.debugm $ "setTargets: " ++ show files
    G.setTargets (map (\t -> t { G.targetAllowObjCode = False }) targets)
    mod_graph <- updateTime targets =<< depanal [] False
    Log.debugm $ "modGraph: " ++ show (map ms_location $ Gap.mgModSummaries mod_graph)
    dflags1 <- getSessionDynFlags
    Log.debugm $ "hidir: " ++ show (hiDir dflags1)
    void $ G.load' LoadAllTargets msg mod_graph


reloadFile :: GhcMonad m => Maybe G.Messager
                         -> (FilePath, FilePath)
                         -> m (Maybe TypecheckedModule, [TypecheckedModule])
reloadFile msg file  = do
  (_, tcs) <- collectASTs $ reloadTarget msg file
  Log.debugm $ "loaded " ++ fst file ++ " - " ++ snd file
  let get_fp = ml_hs_file . ms_location . pm_mod_summary . tm_parsed_module
  Log.debugm $ "Typechecked modules for: " ++ (unlines $ map (show . get_fp) tcs)
  let findMod [] = Nothing
      findMod (x:xs) = case get_fp x of
                         Just fp -> if fp `isSuffixOf` (snd file) then Just x else findMod xs
                         Nothing -> findMod xs
  return (findMod tcs, tcs)

msTargetIs' :: ModSummary -> FilePath -> Bool
msTargetIs' ms f = ml_hs_file (ms_location ms) == Just f

-- | We bump the times for any ModSummary's that are Targets, to
-- fool the recompilation checker so that we can get the typechecked modules
updateTime' :: MonadIO m => (FilePath, FilePath) -> ModuleGraph -> m ModuleGraph
updateTime' (file, mapped_file) graph = liftIO $ do
  cur_time <- getCurrentTime

  let go ms
        | traceShow (msTargetIs' ms file, file, ms_location ms) False = undefined
        | msTargetIs' ms file = ms { ms_hs_date = cur_time
                             , ms_location = (ms_location ms) { ml_hs_file = Just mapped_file }
                             , ms_iface_date = Nothing }
        | otherwise = ms
  pure $ Gap.mapMG go graph

resetFile :: (FilePath, FilePath) -> ModuleGraph -> ModuleGraph
resetFile (file, mapped_file) graph =
  let go ms
        | msTargetIs' ms mapped_file = ms {
                              ms_location = (ms_location ms) { ml_hs_file = Just file } }
        | otherwise = ms
  in Gap.mapMG go graph

-- | Reload a module which is already in the module graph.
reloadTarget :: GhcMonad m => Maybe G.Messager -> (FilePath, FilePath) -> m ()
reloadTarget msg file = do
  -- Invariant -- targets are always just the normal files on disk, apart
  -- from in this function -- where we override one temporarily.
  tgts <- getTargets
  Log.debugm $ "targets" ++ showSDocUnsafe (ppr tgts)
  mod_graph <- updateTime' file =<< depanal [] False
  G.load' LoadAllTargets msg mod_graph
  -- Now need to reset the modified path so we can find the right module
  -- next time we try to do a reload.
  new_mod_graph <- getModuleGraph
  modifySession (\hsc_env -> hsc_env { hsc_mod_graph = resetFile file new_mod_graph })
  G.setTargets tgts

collectASTs :: (GhcMonad m) => m a -> m (a, [TypecheckedModule])
collectASTs action = do
  dflags0 <- getSessionDynFlags
  ref1 <- liftIO $ newIORef []
  let dflags1 = dflags0 { hooks = (hooks dflags0)
                          { hscFrontendHook = Just (astHook ref1) }
                        }
  -- Modify session is much faster than `setSessionDynFlags`.
  modifySession $ \h -> h{ hsc_dflags = dflags1 }
  res <- action
  tcs <- liftIO $ readIORef ref1
  -- Unset the hook so that we don't retain the reference ot the IORef so it can be gced.
  -- This stops the typechecked modules being retained in some cases.
  liftIO $ writeIORef ref1 []
  dflags0 <- getSessionDynFlags
  let dflags2 = dflags1 { hooks = (hooks dflags0)
                          { hscFrontendHook = Nothing }
                        }

  modifySession $ \h -> h{ hsc_dflags = dflags2 }

  return (res, tcs)

astHook :: IORef [TypecheckedModule] -> ModSummary -> Hsc FrontendResult
astHook tc_ref ms = ghcInHsc $ do
  p <- G.parseModule ms
  tcm <- G.typecheckModule p
  let tcg_env = fst (tm_internals_ tcm)
  liftIO $ modifyIORef tc_ref (tcm :)
  return $ FrontendTypecheck tcg_env

ghcInHsc :: Ghc a -> Hsc a
ghcInHsc gm = do
  hsc_session <- getHscEnv
  session <- liftIO $ newIORef hsc_session
  liftIO $ reflectGhc gm (Session session)

{-
overrideTargetMapped :: [Target] -> Target -> FilePath -> [Target]
overrideTargetMapped [] _ _ = []
overrideTargetMapped (t@(Target tid b tc):ts) new_t mapped_fp
  | new_t == tid
  = Target (TargetFile mapped_fp Nothing) b tc : ts
  | otherwise
  = t : overrideTargetMapped ts new_t mapped_fp
  -}


guessTargetMapped :: (GhcMonad m) => (FilePath, FilePath) -> m Target
guessTargetMapped (orig_file_name, mapped_file_name) = do
  t <- G.guessTarget orig_file_name Nothing
  return t --(setTargetFilename mapped_file_name t)

setTargetFilename :: FilePath -> Target -> Target
setTargetFilename fn t =
  t { targetId = case targetId t of
                  TargetFile _ p -> TargetFile fn p
                  tid -> tid }
