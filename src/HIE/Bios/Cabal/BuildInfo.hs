{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HIE.Bios.Cabal.BuildInfo where

import qualified Data.Aeson.Combinators.Decode as ACD
import Data.Maybe
import Data.Either
import Cabal.BuildInfo
import Control.Monad
import System.FilePath
import Data.Foldable (foldr')
import System.Directory

collectBuildInfo :: FilePath -> IO (Maybe BuildInfo)
collectBuildInfo builddir = do
  let planJson = builddir </> "cache" </> "plan.json"
  buildInfos <- ACD.decodeFileStrict buildInfoPathDecoder planJson
  case buildInfos of
    Nothing -> error "TODO: failed to decode plan.json"
    Just bi -> do
      existing <- filterM doesFileExist bi
      realBuildInfos <- mapM decodeBuildInfoFile existing
      case partitionEithers realBuildInfos of
        (errs@(_:_), _) -> error $ "TODO: failed to build-info.json: " ++ unlines errs
        (_, infos) -> pure $ merge infos
  where
    merge :: [BuildInfo] -> Maybe BuildInfo
    merge [] = Nothing
    merge (x:xs) = Just $ foldr' go x xs

    go :: BuildInfo -> BuildInfo -> BuildInfo
    go b1 b2 = b1 { components = components b1 ++ components b2 }

buildInfoPathDecoder :: ACD.Decoder [FilePath]
buildInfoPathDecoder = do
  let buildInfoDecoder = ACD.maybe $ ACD.key "build-info" ACD.string
  catMaybes <$> ACD.key "install-plan" (ACD.list buildInfoDecoder)
