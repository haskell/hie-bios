{-# LANGUAGE BangPatterns, CPP, TypeApplications #-}

module HIE.Bios.Ghc.Logger (
    withLogger
  ) where

import GHC (DynFlags(..), SrcSpan(..), GhcMonad, getSessionDynFlags)
import qualified GHC as G
import Control.Monad.IO.Class

import GHC.Data.Bag
import GHC.Data.FastString (unpackFS)
import GHC.Driver.Session (dopt, DumpFlag(Opt_D_dump_splices))
import GHC.Types.SourceError
import GHC.Utils.Error
import GHC.Utils.Logger

import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.Maybe (fromMaybe)

import System.FilePath (normalise)

import HIE.Bios.Ghc.Doc (showPage, getStyle)
import HIE.Bios.Ghc.Api (withDynFlags)
import qualified HIE.Bios.Ghc.Gap as Gap

import GHC.Types.Error
import GHC.Driver.Errors.Types

----------------------------------------------------------------

type Builder = [String] -> [String]

newtype LogRef = LogRef (IORef Builder)

newLogRef :: IO LogRef
newLogRef = LogRef <$> newIORef id

readAndClearLogRef :: LogRef -> IO String
readAndClearLogRef (LogRef ref) = do
    b <- readIORef ref
    writeIORef ref id
    return $! unlines (b [])

appendLogRef :: DynFlags -> Gap.PprStyle -> LogRef -> LogAction
appendLogRef df style (LogRef ref) _
#if __GLASGOW_HASKELL__ < 905
    (MCDiagnostic sev _)
#else
    (MCDiagnostic sev _ _)
#endif
  src msg = do
        let !l = ppMsg src sev df style msg
        modifyIORef ref (\b -> b . (l:))
appendLogRef _ _ _ _ _ _ _ = pure ()

----------------------------------------------------------------

-- | Set the session flag (e.g. "-Wall" or "-w:") then
--   executes a body. Log messages are returned as 'String'.
--   Right is success and Left is failure.
withLogger ::
  (GhcMonad m)
  => (DynFlags -> DynFlags) -> m () -> m (Either String String)
withLogger setDF body = Gap.handle sourceError $ do
    logref <- liftIO newLogRef
    dflags <- getSessionDynFlags
    style <- getStyle dflags
    G.pushLogHookM (const $ appendLogRef dflags style logref)
    let setLogger _ df = df
    r <- withDynFlags (setLogger logref . setDF) $ do
      body
      liftIO $ Right <$> readAndClearLogRef logref
    G.popLogHookM
    pure r



----------------------------------------------------------------

-- | Converting 'SourceError' to 'String'.
sourceError ::
  (GhcMonad m)
  => SourceError -> m (Either String String)
sourceError err = do
    dflag <- getSessionDynFlags
    style <- getStyle dflag
    let ret = unlines . errBagToStrList dflag style . getMessages . srcErrorMessages $ err
    return (Left ret)

errBagToStrList :: DynFlags -> Gap.PprStyle -> Bag (MsgEnvelope GhcMessage) -> [String]
errBagToStrList dflag style = map (ppErrMsg dflag style) . reverse . bagToList


ppErrMsg :: DynFlags -> Gap.PprStyle -> MsgEnvelope GhcMessage -> String
ppErrMsg dflag style err = ppMsg spn SevError dflag style msg -- ++ ext
   where
     spn = errMsgSpan err
#if __GLASGOW_HASKELL__ >= 905
     msg = pprLocMsgEnvelope (defaultDiagnosticOpts @GhcMessage) err
#else
     msg = pprLocMsgEnvelope err
#endif

ppMsg :: SrcSpan -> G.Severity-> DynFlags -> Gap.PprStyle -> SDoc -> String
ppMsg spn sev dflag style msg = prefix ++ cts
  where
    cts  = showPage dflag style msg
    defaultPrefix
      | isDumpSplices dflag = ""
      | otherwise           = checkErrorPrefix
    prefix = fromMaybe defaultPrefix $ do
        (line,col,_,_) <- getSrcSpan spn
        file <- normalise <$> getSrcFile spn
        let severityCaption = showSeverityCaption sev
        return $ file ++ ":" ++ show line ++ ":" ++ show col ++ ":" ++ severityCaption

checkErrorPrefix :: String
checkErrorPrefix = "Dummy:0:0:Error:"

showSeverityCaption :: G.Severity -> String
showSeverityCaption G.SevWarning = "Warning: "
showSeverityCaption _          = ""

getSrcFile :: SrcSpan -> Maybe String
getSrcFile (Gap.RealSrcSpan spn) = Just . unpackFS . G.srcSpanFile $ spn
getSrcFile _                   = Nothing

isDumpSplices :: DynFlags -> Bool
isDumpSplices dflag = dopt Opt_D_dump_splices dflag

getSrcSpan :: SrcSpan -> Maybe (Int,Int,Int,Int)
getSrcSpan (Gap.RealSrcSpan spn) =
    Just ( G.srcSpanStartLine spn
         , G.srcSpanStartCol spn
         , G.srcSpanEndLine spn
         , G.srcSpanEndCol spn)
getSrcSpan _ = Nothing
