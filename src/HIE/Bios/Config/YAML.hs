{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE CPP                   #-}

-- | Datatypes for parsing @hie.yaml@ files
module HIE.Bios.Config.YAML
  ( CradleConfigYAML(..)
  , CradleComponent(..)
  , MultiSubComponent(..)
  , CabalConfig(..)
  , CabalComponent(..)
  , StackConfig(..)
  , StackComponent(..)
  , DirectConfig(..)
  , BiosConfig(..)
  , NoneConfig(..)
  , OtherConfig(..)
  , OneOrManyComponents(..)
  , Callable(..)
  ) where

import           Control.Applicative ((<|>))
import           Data.Aeson
#if MIN_VERSION_aeson(2,0,0)
import           Data.Aeson.KeyMap   (keys)
#else
import qualified Data.HashMap.Strict as Map
import qualified Data.Text           as T
#endif
import           Data.Aeson.Types    (Parser, typeMismatch)
import qualified Data.Char           as C (toLower)
import           Data.List           ((\\))
import           GHC.Generics        (Generic)

#if !MIN_VERSION_aeson(2,0,0)
-- | Backwards compatible type-def for Key
-- This used to be just a Text, but since aeson >= 2
-- this is an opaque datatype.
type Key = T.Text
-- | Backwards compatible type-def for KeyMap
-- This used to be just a HashMap, but since aeson >= 2
-- this is an opaque datatype.
type KeyMap v = Map.HashMap T.Text v

keys :: KeyMap v -> [Key]
keys = Map.keys
#endif

checkObjectKeys :: [Key] -> Object -> Parser ()
checkObjectKeys allowedKeys obj =
  let extraKeys = keys obj \\ allowedKeys
   in case extraKeys of
        []          -> pure ()
        _           -> fail $ mconcat [ "Unexpected keys "
                                      , show extraKeys
                                      , ", keys allowed: "
                                      , show allowedKeys
                                      ]

data CradleConfigYAML a
  = CradleConfigYAML { cradle       :: CradleComponent a
                     , dependencies :: Maybe [FilePath]
                     } deriving (Generic, FromJSON)

data CradleComponent a
  = Multi [MultiSubComponent a]
  | Cabal CabalConfig
  | Stack StackConfig
  | Direct DirectConfig
  | Bios BiosConfig
  | None NoneConfig
  | Other (OtherConfig a)
  deriving (Generic)

instance FromJSON a => FromJSON (CradleComponent a) where
  parseJSON = let opts = defaultOptions { constructorTagModifier = fmap C.toLower
                                        , sumEncoding = ObjectWithSingleField
                                        }
               in genericParseJSON opts

data NoneConfig = NoneConfig

data OtherConfig a
  = OtherConfig { otherConfig       :: a
                , originalYamlValue :: Value
                }

instance FromJSON a => FromJSON (OtherConfig a) where
  parseJSON v = OtherConfig
                  <$> parseJSON v
                  <*> pure v

instance FromJSON NoneConfig where
  parseJSON Null = pure NoneConfig
  parseJSON v    = typeMismatch "NoneConfig" v

data MultiSubComponent a
  = MultiSubComponent { path   :: FilePath
                      , config :: CradleConfigYAML a
                      } deriving (Generic, FromJSON)

data CabalConfig
  = CabalConfig { cabalProject    :: Maybe FilePath
                , cabalComponents :: OneOrManyComponents CabalComponent
                }

instance FromJSON CabalConfig where
  parseJSON v@(Array _)     = CabalConfig Nothing . ManyComponents <$> parseJSON v
  parseJSON v@(Object obj)  = (checkObjectKeys ["cabalProject", "component", "components"] obj)
                                *> (CabalConfig
                                  <$> obj .:? "cabalProject"
                                  <*> parseJSON v)
  parseJSON Null            = pure $ CabalConfig Nothing NoComponent
  parseJSON v               = typeMismatch "CabalConfig" v

data CabalComponent
  = CabalComponent { cabalPath      :: FilePath
                   , cabalComponent :: String
                   , cabalComponentProject :: Maybe FilePath
                   }

instance FromJSON CabalComponent where
  parseJSON =
    let parseCabalComponent obj = checkObjectKeys ["path", "component", "cabalProject"] obj
                                    *> (CabalComponent
                                          <$> obj .: "path"
                                          <*> obj .: "component"
                                          <*> obj .:? "cabalProject"
                                        )
     in withObject "CabalComponent" parseCabalComponent

data StackConfig
  = StackConfig { stackYaml       :: Maybe FilePath
                , stackComponents :: OneOrManyComponents StackComponent
                }

data StackComponent
  = StackComponent { stackPath          :: FilePath
                   , stackComponent     :: String
                   , stackComponentYAML :: Maybe FilePath
                   }

instance FromJSON StackConfig where
  parseJSON v@(Array _)     = StackConfig Nothing . ManyComponents <$> parseJSON v
  parseJSON v@(Object obj)  = (checkObjectKeys ["component", "components", "stackYaml"] obj)
                                *> (StackConfig
                                      <$> obj .:? "stackYaml"
                                      <*> parseJSON v
                                    )
  parseJSON Null            = pure $ StackConfig Nothing NoComponent
  parseJSON v               = typeMismatch "StackConfig" v

instance FromJSON StackComponent where
  parseJSON =
    let parseStackComponent obj = (checkObjectKeys ["path", "component", "stackYaml"] obj)
                                    *> (StackComponent
                                          <$> obj .: "path"
                                          <*> obj .: "component"
                                          <*> obj .:? "stackYaml")
     in withObject "StackComponent" parseStackComponent

data OneOrManyComponents component
  = SingleComponent String
  | ManyComponents [component]
  | NoComponent

instance FromJSON component => FromJSON (OneOrManyComponents component) where
  parseJSON =
    let parseComponents o = (parseSingleComponent o <|> parseSubComponents o <|> pure NoComponent)
        parseSingleComponent o = SingleComponent <$> o .: "component"
        parseSubComponents   o = ManyComponents <$> o .: "components"
     in withObject "Components" parseComponents

data DirectConfig
  = DirectConfig { arguments :: [String] }
  deriving (Generic, FromJSON)

data BiosConfig =
  BiosConfig { callable     :: Callable
             , depsCallable :: Maybe Callable
             , ghcPath      :: Maybe FilePath
             }

instance FromJSON BiosConfig where
  parseJSON = withObject "BiosConfig" parseBiosConfig

data Callable
  = Program FilePath
  | Shell String

parseBiosConfig :: Object -> Parser BiosConfig
parseBiosConfig obj =
  let parseCallable o = (Program <$> o .: "program") <|> (Shell <$> o .: "shell")
      parseDepsCallable o = (Just . Program <$> o .: "dependency-program")
                            <|> (Just . Shell <$> o .: "dependency-shell")
                            <|> (pure Nothing)
      parse o = BiosConfig  <$> parseCallable o
                            <*> parseDepsCallable o
                            <*> (o .:? "with-ghc")
      check = checkObjectKeys ["program", "shell", "dependency-program", "dependency-shell", "with-ghc"]
   in check obj *> parse obj
