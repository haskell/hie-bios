{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
-- | Logic and datatypes for parsing @hie.yaml@ files.
module HIE.Bios.Config(
    readConfig,
    Config(..),
    CradleConfig(..),
    CradleType(..)
    ) where

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as Map
import           Data.Foldable (foldrM)
import           Data.Yaml

data CradleConfig a =
    CradleConfig
        { cradleDependencies :: [FilePath]
        -- ^ Dependencies of a cradle.
        -- Dependencies are expected to be relative to the root directory.
        -- The given files are not required to exist.
        , cradleType :: CradleType a
        -- ^ Type of the cradle to use. Actions to obtain
        -- compiler flags from are dependant on this field.
        }
        deriving (Show, Eq)

data CradleType a
    = Cabal { component :: Maybe String }
    | CabalMulti [ (FilePath, String) ]
    | Stack { component :: Maybe String }
    | StackMulti [ (FilePath, String) ]
--  Bazel and Obelisk used to be supported but bit-rotted and no users have complained.
--  They can be added back if a user
--    | Bazel
--    | Obelisk
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
    | None
    | Multi [ (FilePath, CradleConfig a) ]
    | Other { otherConfig :: a }
    deriving (Show, Eq)

instance FromJSON a => FromJSON (CradleType a) where
    parseJSON (Object o) = parseCradleType o
    parseJSON _ = fail "Not a known cradle type. Possible are: cabal, stack, bios, direct, default, none, multi"

parseCradleType :: FromJSON a => Object -> Parser (CradleType a)
parseCradleType o
    | Just val <- Map.lookup "cabal" o = parseCabal val
    | Just val <- Map.lookup "stack" o = parseStack val
--    | Just _val <- Map.lookup "bazel" o = return Bazel
--    | Just _val <- Map.lookup "obelisk" o = return Obelisk
    | Just val <- Map.lookup "bios" o = parseBios val
    | Just val <- Map.lookup "direct" o = parseDirect val
    | Just _val <- Map.lookup "none" o = return None
    | Just val  <- Map.lookup "multi" o = parseMulti val
    | Just val  <- Map.lookup "other" o = Other <$> parseJSON val
parseCradleType o = fail $ "Unknown cradle type: " ++ show o

parseStackOrCabal
  :: (Maybe String -> CradleType a)
  -> ([(FilePath, String)] -> CradleType a)
  -> Value
  -> Parser (CradleType a)
parseStackOrCabal singleConstructor _ (Object x)
  | Map.size x == 1, Just (String stackComponent) <- Map.lookup "component" x
  = return $ singleConstructor $ Just $ T.unpack stackComponent
  | Map.null x
  = return $ singleConstructor Nothing
  | otherwise
  = fail "Not a valid Configuration type, following keys are allowed: component"
parseStackOrCabal _ multiConstructor (Array x) = do
  let parseOne e
        | Object v <- e
        , Just (String prefix) <- Map.lookup "path" v
        , Just (String comp) <- Map.lookup "component" v
        , Map.size v == 2
        = return (T.unpack prefix, T.unpack comp)
        | otherwise
        = fail "Expected an object with path and component keys"

  xs <- foldrM (\v cs -> (: cs) <$> parseOne v) [] x
  return $ multiConstructor xs
parseStackOrCabal singleConstructor _ Null = return $ singleConstructor Nothing
parseStackOrCabal _ _ _ = fail "Configuration is expected to be an object."

parseStack :: Value -> Parser (CradleType a)
parseStack = parseStackOrCabal Stack StackMulti

parseCabal :: Value -> Parser (CradleType a)
parseCabal = parseStackOrCabal Cabal CabalMulti

parseBios :: Value -> Parser (CradleType a)
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

parseDirect :: Value -> Parser (CradleType a)
parseDirect (Object x)
    | Map.size x == 1
    , Just (Array v) <- Map.lookup "arguments" x
    = return $ Direct [T.unpack s | String s <- V.toList v]

    | otherwise
    = fail "Not a valid Direct Configuration type, following keys are allowed: arguments"
parseDirect _ = fail "Direct Configuration is expected to be an object."

parseMulti :: FromJSON a => Value -> Parser (CradleType a)
parseMulti (Array x)
    = Multi <$> mapM parsePath (V.toList x)
parseMulti _ = fail "Multi Configuration is expected to be an array."

parsePath :: FromJSON a => Value -> Parser (FilePath, CradleConfig a)
parsePath (Object v)
  | Just (String path) <- Map.lookup "path" v
  , Just c <- Map.lookup "config" v
  = (T.unpack path,) <$> parseJSON c
parsePath o = fail ("Multi component is expected to be an object." ++ show o)

-- | Configuration that can be used to load a 'Cradle'.
-- A configuration has roughly the following form:
--
-- @
-- cradle:
--   cabal:
--     component: "lib:hie-bios"
-- @
newtype Config a = Config { cradle :: CradleConfig a }
    deriving (Show, Eq)

instance FromJSON a => FromJSON (CradleConfig a) where
    parseJSON (Object val) = do
            crd     <- val .: "cradle"
            crdDeps <- case Map.size val of
                1 -> return []
                2 -> val .: "dependencies"
                _ -> fail "Unknown key, following keys are allowed: cradle, dependencies"

            return $ CradleConfig { cradleType         = crd
                                  , cradleDependencies = crdDeps
                                  }

    parseJSON _ = fail "Expected a cradle: key containing the preferences, possible values: cradle, dependencies"


instance FromJSON a => FromJSON (Config a) where
    parseJSON o = Config <$> parseJSON o


-- | Decode given file to a 'Config a' value.
-- Type variable 'a' can be used to extend the 'hie.yaml' file format
-- to extend configuration in the user-library.
-- If the contents of the file is not a valid 'Config a',
-- an 'Control.Exception.IOException' is thrown.
readConfig :: FromJSON a => FilePath -> IO (Config a)
readConfig = decodeFileThrow