{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module HIE.Bios.Cradle.ProgramVersions
  ( ProgramVersions(..)
  , makeVersions
  , runCachedIO
  ) where


import HIE.Bios.Types
import qualified HIE.Bios.Process as Process
import HIE.Bios.Cradle.Utils (trim)

import Colog.Core (LogAction (..), WithSeverity (..))
import Data.Version
import Data.IORef
import Text.ParserCombinators.ReadP (readP_to_S)

data ProgramVersions =
  ProgramVersions { cabalVersion  :: CachedIO (Maybe Version)
                  , stackVersion  :: CachedIO (Maybe Version)
                  , ghcVersion    :: CachedIO (Maybe Version)
                  }

newtype CachedIO a = CachedIO (IORef (Either (IO a) a))

makeCachedIO :: IO a -> IO (CachedIO a)
makeCachedIO act = CachedIO <$> newIORef (Left act)

runCachedIO :: CachedIO a -> IO a
runCachedIO (CachedIO ref) =
  readIORef ref >>= \case
    Right x -> pure x
    Left act -> do
      x <- act
      writeIORef ref (Right x)
      pure x

makeVersions :: LogAction IO (WithSeverity Log) -> FilePath -> ([String] -> IO (CradleLoadResult String)) -> IO ProgramVersions
makeVersions l wdir ghc = do
  cabalVersion <- makeCachedIO $ getCabalVersion l wdir
  stackVersion <- makeCachedIO $ getStackVersion l wdir
  ghcVersion   <- makeCachedIO $ getGhcVersion ghc
  pure ProgramVersions{..}

getCabalVersion :: LogAction IO (WithSeverity Log) -> FilePath -> IO (Maybe Version)
getCabalVersion l wdir = do
  res <- Process.readProcessWithCwd l wdir "cabal" ["--numeric-version"] ""
  case res of
    CradleSuccess stdo ->
      pure $ versionMaybe stdo
    _ -> pure Nothing

getStackVersion :: LogAction IO (WithSeverity Log) -> FilePath -> IO (Maybe Version)
getStackVersion l wdir = do
  res <- Process.readProcessWithCwd l wdir "stack" ["--numeric-version"] ""
  case res of
    CradleSuccess stdo ->
      pure $ versionMaybe stdo
    _ -> pure Nothing

getGhcVersion :: ([String] -> IO (CradleLoadResult String)) -> IO (Maybe Version)
getGhcVersion ghc = do
  res <- ghc ["--numeric-version"]
  case res of
    CradleSuccess stdo ->
      pure $ versionMaybe stdo
    _ -> pure Nothing

versionMaybe :: String -> Maybe Version
versionMaybe xs = case reverse $ readP_to_S parseVersion (trim xs) of
  [] -> Nothing
  (x:_) -> Just (fst x)
