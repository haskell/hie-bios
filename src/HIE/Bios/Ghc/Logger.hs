{-# LANGUAGE BangPatterns, CPP #-}

module HIE.Bios.Ghc.Logger (
    withLogger
  ) where

import Bag (Bag, bagToList)
import CoreMonad (liftIO)
import DynFlags (LogAction, dopt, DumpFlag(Opt_D_dump_splices))
import ErrUtils
import FastString (unpackFS)
import GHC (DynFlags(..), SrcSpan(..), GhcMonad)
import qualified GHC as G
import HscTypes (SourceError, srcErrorMessages)
import Outputable (PprStyle, SDoc)

import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.Maybe (fromMaybe)
import System.FilePath (normalise)

import HIE.Bios.Ghc.Doc (showPage, getStyle)
import HIE.Bios.Ghc.Api (withDynFlags)
import qualified HIE.Bios.Ghc.Gap as Gap

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

appendLogRef :: DynFlags -> PprStyle -> LogRef -> LogAction
appendLogRef df style (LogRef ref) _ _ sev src
#if __GLASGOW_HASKELL__ < 900
  _style
#endif
  msg = do
        let !l = ppMsg src sev df style msg
        modifyIORef ref (\b -> b . (l:))

----------------------------------------------------------------

-- | Set the session flag (e.g. "-Wall" or "-w:") then
--   executes a body. Log messages are returned as 'String'.
--   Right is success and Left is failure.
withLogger ::
  (GhcMonad m)
  => (DynFlags -> DynFlags) -> m () -> m (Either String String)
withLogger setDF body = Gap.handle sourceError $ do
    logref <- liftIO newLogRef
    dflags <- G.getSessionDynFlags
    style <- getStyle dflags
    let setLogger logref df = df { log_action =  appendLogRef df style logref }
    withDynFlags (setLogger logref . setDF) $ do
      body
      liftIO $ Right <$> readAndClearLogRef logref

----------------------------------------------------------------

-- | Converting 'SourceError' to 'String'.
sourceError ::
  (GhcMonad m)
  => SourceError -> m (Either String String)
sourceError err = do
    dflag <- G.getSessionDynFlags
    style <- getStyle dflag
    let ret = unlines . errBagToStrList dflag style . srcErrorMessages $ err
    return (Left ret)

errBagToStrList :: DynFlags -> PprStyle -> Bag ErrMsg -> [String]
errBagToStrList dflag style = map (ppErrMsg dflag style) . reverse . bagToList

----------------------------------------------------------------

ppErrMsg :: DynFlags -> PprStyle -> ErrMsg -> String
ppErrMsg dflag style err = ppMsg spn SevError dflag style msg -- ++ ext
   where
     spn = errMsgSpan err
     msg = pprLocErrMsg err
     -- fixme
--     ext = showPage dflag style (pprLocErrMsg $ errMsgReason err)

ppMsg :: SrcSpan -> Severity-> DynFlags -> PprStyle -> SDoc -> String
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

showSeverityCaption :: Severity -> String
showSeverityCaption SevWarning = "Warning: "
showSeverityCaption _          = ""

getSrcFile :: SrcSpan -> Maybe String
getSrcFile (Gap.RealSrcSpan spn) = Just . unpackFS . G.srcSpanFile $ spn
getSrcFile _                   = Nothing

isDumpSplices :: DynFlags -> Bool
isDumpSplices dflag = dopt Opt_D_dump_splices dflag

getSrcSpan :: SrcSpan -> Maybe (Int,Int,Int,Int)
getSrcSpan (Gap.RealSrcSpan spn) = Just ( G.srcSpanStartLine spn
                                    , G.srcSpanStartCol spn
                                    , G.srcSpanEndLine spn
                                    , G.srcSpanEndCol spn)
getSrcSpan _ = Nothing
