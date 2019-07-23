{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module HIE.Bios.Load ( loadFileWithMessage, loadFile, setTargetFiles, setTargetFilesWithMessage) where

import CoreMonad (liftIO)
import GHC
import qualified GHC as G
import qualified GhcMake as G
import qualified HscMain as G
import HscTypes
import Outputable
import Control.Monad.IO.Class

import Data.IORef

import System.Directory
import Hooks
import TcRnTypes (FrontendResult(..))
import Control.Monad (forM, void)
import GhcMonad
import HscMain
import Debug.Trace
import Data.List

import Data.Time.Clock

#if __GLASGOW_HASKELL__ < 806
pprTraceM x s = pprTrace x s (return ())
#endif

-- | Obtaining type of a target expression. (GHCi's type:)
loadFileWithMessage :: GhcMonad m
         => Maybe G.Messager
         -> (FilePath, FilePath)     -- ^ A target file.
         -> m (Maybe TypecheckedModule, [TypecheckedModule])
loadFileWithMessage msg file = do
  dir <- liftIO $ getCurrentDirectory
  pprTraceM "loadFile:2" (text dir)
  df <- getSessionDynFlags
  pprTraceM "loadFile:3" (ppr $ optLevel df)
  (_, tcs) <- collectASTs $ do
    (setTargetFilesWithMessage msg [file])
  pprTraceM "loaded" (text (fst file) $$ text (snd file))
  let get_fp = ml_hs_file . ms_location . pm_mod_summary . tm_parsed_module
  traceShowM ("tms", (map get_fp tcs))
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
  pure $ mapMG go graph

-- | Set the files as targets and load them.
setTargetFilesWithMessage :: (GhcMonad m)  => Maybe G.Messager -> [(FilePath, FilePath)] -> m ()
setTargetFilesWithMessage msg files = do
    targets <- forM files guessTargetMapped
    pprTrace "setTargets" (vcat (map (\(a,b) -> parens $ text a <+> text "," <+> text b) files) $$ ppr targets) (return ())
    G.setTargets (map (\t -> t { G.targetAllowObjCode = False }) targets)
    mod_graph <- updateTime targets =<< depanal [] False
    pprTrace "modGraph" (ppr $ mgModSummaries mod_graph) (return ())
    pprTrace "modGraph" (ppr $ map ms_location $ mgModSummaries mod_graph) (return ())
    dflags1 <- getSessionDynFlags
    pprTrace "hidir" (ppr $ hiDir dflags1) (return ())
    void $ G.load' LoadAllTargets msg mod_graph

collectASTs :: (GhcMonad m) => m a -> m (a, [TypecheckedModule])
collectASTs action = do
  dflags0 <- getSessionDynFlags
  ref1 <- liftIO $ newIORef []
  let dflags1 = dflags0 { hooks = (hooks dflags0)
                          { hscFrontendHook = traceShow "Use hook" $ Just (astHook ref1) }
                        }
  void $ setSessionDynFlags $ dflags1 -- gopt_set dflags1 Opt_ForceRecomp
  res <- action
  tcs <- liftIO $ readIORef ref1
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


guessTargetMapped :: (GhcMonad m) => (FilePath, FilePath) -> m Target
guessTargetMapped (orig_file_name, mapped_file_name) = do
  t <- G.guessTarget orig_file_name Nothing
  return (setTargetFilename mapped_file_name t)

setTargetFilename :: FilePath -> Target -> Target
setTargetFilename fn t =
  t { targetId = case targetId t of
                  TargetFile _ p -> TargetFile fn p
                  tid -> tid }
