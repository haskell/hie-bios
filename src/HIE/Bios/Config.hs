{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module HIE.Bios.Config(
    readConfig,
    Config(..),
    CradleConfig(..)
    ) where

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as Map
import Data.Yaml


data CradleConfig = Cabal { component :: Maybe String }
                  | Stack
                  | Bazel
                  | Obelisk
                  | Bios { prog :: FilePath }
                  | Direct { arguments :: [String] }
                  | Default
                  deriving (Show)

instance FromJSON CradleConfig where
    parseJSON (Object (Map.toList -> [(key, val)]))
        | key == "cabal" = case val of
            Object x | Just (String v) <- Map.lookup "component" x -> return $ Cabal $ Just $ T.unpack v
            _ -> return $ Cabal Nothing
        | key == "stack" = return Stack
        | key == "bazel" = return Bazel
        | key == "obelisk" = return Obelisk
        | key == "bios", Object x <- val, Just (String v) <- Map.lookup "program" x = return $ Bios $ T.unpack v
        | key == "direct", Object x <- val, Just (Array v) <- Map.lookup "arguments" x = return $ Direct [T.unpack s | String s <- V.toList v]
        | key == "default" = return Default
    parseJSON _ = fail "Not a known configuration"

data Config = Config { cradle :: CradleConfig }
    deriving (Show)

instance FromJSON Config where
    parseJSON (Object (Map.toList -> [("cradle", x)])) = Config <$> parseJSON x
    parseJSON _ = fail "Expected a cradle: key containing the preferences"

readConfig :: FilePath -> IO Config
readConfig fp = decodeFileThrow fp
