{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
-- | Logic and datatypes for parsing @hie.yaml@ files.
module HIE.Bios.Config(
    readConfig,
    Config(..),
    CradleConfig(..),
    CabalType,
    pattern CabalType,
    cabalComponent,
    StackType,
    pattern StackType,
    stackComponent,
    stackYaml,
    CradleType(..),
    Callable(..)
    ) where

import Control.Exception
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.Monoid (Last(..))
import           Data.Foldable (foldrM)
import           Data.Aeson (JSONPath)
import           Data.Yaml
import           Data.Yaml.Internal (Warning(..))

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

data CabalType
    = CabalType_ { _cabalComponent :: !(Last String) }
    deriving (Eq)

instance Semigroup CabalType where
    CabalType_ cr <> CabalType_ cl = CabalType_ (cr <> cl)

instance Monoid CabalType where
    mempty = CabalType_ mempty

pattern CabalType :: Maybe String -> CabalType
pattern CabalType { cabalComponent } = CabalType_ (Last cabalComponent)
{-# COMPLETE CabalType #-}

instance Show CabalType where
  show = show . Cabal

data StackType
    = StackType_ { _stackComponent :: !(Last String) , _stackYaml :: !(Last String) }
    deriving (Eq)

instance Semigroup StackType where
    StackType_ cr yr <> StackType_ cl yl = StackType_ (cr <> cl) (yr <> yl)

instance Monoid StackType where
    mempty = StackType_ mempty mempty

pattern StackType :: Maybe String -> Maybe String -> StackType
pattern StackType { stackComponent, stackYaml } = StackType_ (Last stackComponent) (Last stackYaml)
{-# COMPLETE StackType #-}

instance Show StackType where
  show = show . Stack

data CradleType a
    = Cabal { cabalType :: !CabalType }
    | CabalMulti { defaultCabal :: !CabalType, subCabalComponents :: [ (FilePath, CabalType) ] }
    | Stack { stackType :: !StackType }
    | StackMulti { defaultStack :: !StackType, subStackComponents :: [ (FilePath, StackType) ] }
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
        , ghcPath :: Maybe FilePath
        -- ^ Optional path to the ghc binary
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
    show (Cabal comp) = "Cabal {component = " ++ show (cabalComponent comp) ++ "}"
    show (CabalMulti d a) = "CabalMulti {defaultCabal = " ++ show d ++ ", subCabalComponents = " ++ show a ++ "}"
    show (Stack comp) = "Stack {component = " ++ show (stackComponent comp) ++ ", stackYaml = " ++ show (stackYaml comp) ++ "}"
    show (StackMulti d a) = "StackMulti {defaultStack = " ++ show d ++ ", subStackComponents = "  ++ show a ++ "}"
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

parseSingleOrMultiple
  :: Monoid x
  => (x -> CradleType a)
  -> (x -> [(FilePath, x)] -> CradleType a)
  -> (Map.HashMap T.Text Value -> Parser x)
  -> Value
  -> Parser (CradleType a)
parseSingleOrMultiple single multiple parse = doParse where
    parseOne e
        | Object v <- e
        , Just (String prefix) <- Map.lookup "path" v
        = (T.unpack prefix,) <$> parse (Map.delete "path" v)
        | otherwise
        = fail "Expected an object with a path key"
    parseArray = foldrM (\v cs -> (: cs) <$> parseOne v) []
    doParse (Object v)
        | Just (Array x) <- Map.lookup "components" v
        = do
            d <- parse (Map.delete "components" v)
            xs <- parseArray x
            return $ multiple d xs
        | Just _ <- Map.lookup "components" v
        = fail "Expected components to be an array of subcomponents"
        | Nothing <- Map.lookup "components" v
        = single <$> parse v
    doParse (Array x)
        = do
            xs <- parseArray x
            return $ multiple mempty xs
    doParse Null = single <$> parse Map.empty
    doParse _ = fail "Configuration is expected to be an object or an array of objects."

parseStack :: Value -> Parser (CradleType a)
parseStack = parseSingleOrMultiple Stack StackMulti $
  \case x | Map.size x == 2
          , Just (String component) <- Map.lookup "component" x
          , Just (String syaml) <- Map.lookup "stackYaml" x
          -> return $ StackType (Just $ T.unpack component) (Just $ T.unpack syaml)
          | Map.size x == 1, Just (String component) <- Map.lookup "component" x
          -> return $ StackType (Just $ T.unpack component) Nothing
          | Map.size x == 1, Just (String syaml) <- Map.lookup "stackYaml" x
          -> return $ StackType Nothing (Just $ T.unpack syaml)
          | Map.null x
          -> return $ StackType Nothing Nothing
          | otherwise
          -> fail "Not a valid Stack configuration, following keys are allowed: component, stackYaml"

parseCabal :: Value -> Parser (CradleType a)
parseCabal = parseSingleOrMultiple Cabal CabalMulti $
  \case x | Map.size x == 1, Just (String component) <- Map.lookup "component" x
          -> return $ CabalType (Just $ T.unpack component)
          | Map.null x
          -> return $ CabalType Nothing
          | otherwise
          -> fail "Not a valid Cabal configuration, following keys are allowed: component"

parseBios :: Value -> Parser (CradleType a)
parseBios (Object x) =
    case biosCallable of
        Just bc -> return $ Bios bc biosDepsCallable ghcPath
        _ -> fail $ "Not a valid Bios Configuration type, following keys are allowed:" ++
                    "program or shell, dependency-program or dependency-shell, with-ghc"
    where
        biosCallable =
            exclusive
                (stringTypeFromMap Program "program")
                (stringTypeFromMap Command "shell")
        biosDepsCallable =
            exclusive
                (stringTypeFromMap Program "dependency-program")
                (stringTypeFromMap Command "dependency-shell")
        ghcPath =
            stringTypeFromMap id "with-ghc"

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
readConfig fp = do
    result <- decodeFileWithWarnings fp
    either throwIO failOnAnyDuplicate result
    where
        failOnAnyDuplicate :: ([Warning], Config a) -> IO (Config a)
        failOnAnyDuplicate (warnings, config) = do
            _ <- case mapMaybe failOnDuplicate warnings of
                    dups@(_:_) -> throwIO $ InvalidYaml $ Just $ YamlException
                                          $ "Duplicate keys are not allowed, found: " ++ show dups
                    _ -> return ()
            return config
        -- future proofing in case more warnings are added
        failOnDuplicate :: Warning -> Maybe JSONPath
        failOnDuplicate (DuplicateKey a) = Just a
