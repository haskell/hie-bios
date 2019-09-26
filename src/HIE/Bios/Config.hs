{-# LANGUAGE OverloadedStrings #-}
module HIE.Bios.Config(
    readConfig,
    Config(..),
    CradleConfig(..),
    CradleType(..)
    ) where

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as Map
import           Data.Yaml

data CradleConfig =
    CradleConfig
        { cradleDependencies :: [FilePath]
        -- ^ Dependencies of a cradle.
        -- Dependencies are expected to be relative to the root directory.
        -- The given files are not required to exist.
        , cradleType :: CradleType
        -- ^ Type of the cradle to use. Actions to obtain
        -- compiler flags from are dependant on this field.
        }
        deriving (Show, Eq)

data CradleType
    = Cabal { component :: Maybe String }
    | Stack
    | Bazel
    | Obelisk
    | Bios
        { prog :: FilePath
        -- ^ Path to program that retrieves options to compile a file
        , depsProg :: Maybe FilePath
        -- ^ Optional Path to program to obtain cradle dependencies.
        -- Each cradle dependency is to be expected to be on a separate line
        -- and relative to the root dir of the cradle, not relative
        -- to the location of this program.
        }
    | Direct { arguments :: [String] }
    | Default
    | None
    deriving (Show, Eq)

instance FromJSON CradleType where
    parseJSON (Object o) = parseCradleType o
    parseJSON _ = fail "Not a known cradle type. Possible are: cabal, stack, bazel, obelisk, bios, direct, default"

parseCradleType :: Object -> Parser CradleType
parseCradleType o
    | Just val <- Map.lookup "cabal" o = parseCabal val
    | Just _val <- Map.lookup "stack" o = return Stack
    | Just _val <- Map.lookup "bazel" o = return Bazel
    | Just _val <- Map.lookup "obelisk" o = return Obelisk
    | Just val <- Map.lookup "bios" o = parseBios val
    | Just val <- Map.lookup "direct" o = parseDirect val
    | Just _val <- Map.lookup "default" o = return Default
    | Just _val <- Map.lookup "none" o = return None
parseCradleType o = fail $ "Unknown cradle type: " ++ show o

parseCabal :: Value -> Parser CradleType
parseCabal (Object x)
    | Map.size x == 1
    , Just (String cabalComponent) <- Map.lookup "component" x
    = return $ Cabal $ Just $ T.unpack cabalComponent

    | Map.null x
    = return $ Cabal Nothing

    | otherwise
    = fail "Not a valid Cabal Configuration type, following keys are allowed: component"
parseCabal _ = fail "Cabal Configuration is expected to be an object."

parseBios :: Value -> Parser CradleType
parseBios (Object x)
    | 2 == Map.size x
    , Just (String biosProgram) <- Map.lookup "program" x
    , Just (String biosDepsProgram) <- Map.lookup "dependency-program" x
    = return $ Bios (T.unpack biosProgram) (Just (T.unpack biosDepsProgram))

    | 1 == Map.size x
    , Just (String biosProgram) <- Map.lookup "program" x
    = return $ Bios (T.unpack biosProgram) Nothing

    | otherwise
    = fail "Not a valid Bios Configuration type, following keys are allowed: program, dependency-program"
parseBios _ = fail "Bios Configuration is expected to be an object."

parseDirect :: Value -> Parser CradleType
parseDirect (Object x)
    | Map.size x == 1
    , Just (Array v) <- Map.lookup "arguments" x
    = return $ Direct [T.unpack s | String s <- V.toList v]

    | otherwise
    = fail "Not a valid Direct Configuration type, following keys are allowed: arguments"
parseDirect _ = fail "Direct Configuration is expected to be an object."

data Config = Config { cradle :: CradleConfig }
    deriving (Show, Eq)

instance FromJSON Config where
    parseJSON (Object val) = do
            crd     <- val .: "cradle"
            crdDeps <- case Map.size val of
                1 -> return []
                2 -> val .: "dependencies"
                _ -> fail "Unknown key, following keys are allowed: cradle, dependencies"

            return Config
                { cradle = CradleConfig { cradleType         = crd
                                        , cradleDependencies = crdDeps
                                        }
                }

    parseJSON _ = fail "Expected a cradle: key containing the preferences, possible values: cradle, dependencies"

readConfig :: FilePath -> IO Config
readConfig = decodeFileThrow
