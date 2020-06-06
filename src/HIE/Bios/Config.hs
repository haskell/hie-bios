{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
-- | Logic and datatypes for parsing @hie.yaml@ files.
module HIE.Bios.Config(
    readConfig,
    Config(..),
    CradleConfig(..),
    CradleType(..),
    Callable(..)
    ) where

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as Map
import           Data.Foldable (foldrM)
import           Data.Yaml

-- | Configuration that can be used to load a 'Cradle'.
-- A configuration has roughly the following form:
--
-- @
-- cradle:
--   cabal:
--     component: "lib:hie-bios"
-- @
newtype Config a = Config { cradle :: CradleConfig a }
    deriving (Show, Eq, Functor)

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
        deriving (Show, Eq, Functor)

data Callable = Program FilePath | Command String
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
        { call :: Callable
        -- ^ Path to program or shell command that retrieves options to compile a file
        , depsCall :: Maybe Callable
        -- ^ Optional path to program or shell command to obtain cradle dependencies.
        -- Each cradle dependency is to be expected to be on a separate line
        -- and relative to the root dir of the cradle.
        }
    | Direct { arguments :: [String] }
    | None
    | Multi [ (FilePath, CradleConfig a) ]
    | Other { otherConfig :: a, originalYamlValue :: Value }
    deriving (Eq, Functor)

instance FromJSON a => FromJSON (CradleType a) where
    parseJSON (Object o) = parseCradleType o
    parseJSON _ = fail "Not a known cradle type. Possible are: cabal, stack, bios, direct, default, none, multi"

instance Show (CradleType a) where
    show (Cabal comp) = "Cabal {component = " ++ show comp ++ "}"
    show (CabalMulti a) = "CabalMulti " ++ show a
    show (Stack comp) = "Stack {component = " ++ show comp ++ "}"
    show (StackMulti a) = "StackMulti " ++ show a
    show Bios { call, depsCall } = "Bios {call = " ++ show call ++ ", depsCall = " ++ show depsCall ++ "}"
    show (Direct args) = "Direct {arguments = " ++ show args ++ "}"
    show None = "None"
    show (Multi a) = "Multi " ++ show a
    show (Other _ val) = "Other {originalYamlValue = " ++ show val ++ "}"

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
    | Just val  <- Map.lookup "other" o = Other <$> parseJSON val <*> pure val
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
    , Just biosCallable <- exclusive (stringTypeFromMap Program "program") (stringTypeFromMap Command "shell")
    , Just biosDepsCallable <- exclusive (stringTypeFromMap Program "dependency-program") (stringTypeFromMap Command "dependency-shell")
    = return $ Bios biosCallable (Just biosDepsCallable)

    | 1 == Map.size x
    , Just biosCallable <- exclusive (stringTypeFromMap Program "program") (stringTypeFromMap Command "shell")
    = return $ Bios biosCallable Nothing

    | otherwise
    = fail "Not a valid Bios Configuration type, following keys are allowed: program or shell, dependency-program or dependency-shell"

    where
        exclusive :: Maybe a -> Maybe a -> Maybe a
        exclusive (Just _) (Just _) = Nothing
        exclusive l Nothing = l
        exclusive Nothing r = r
        stringTypeFromMap :: (String -> t) -> T.Text -> Maybe t
        stringTypeFromMap constructor name = constructor <$> (intoString =<< Map.lookup name x)
        intoString :: Value -> Maybe String
        intoString (String s) = Just (T.unpack s)
        intoString _ = Nothing

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