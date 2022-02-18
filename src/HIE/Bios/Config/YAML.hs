{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

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
import           Data.Aeson.Types    (Object, Parser, Value (Null),
                                      typeMismatch)
import qualified Data.Char           as C (toLower)
import           GHC.Generics        (Generic)

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
  = CabalConfig { cabalComponents :: OneOrManyComponents CabalComponent }

instance FromJSON CabalConfig where
  parseJSON v@(Array _)  = CabalConfig . ManyComponents <$> parseJSON v
  parseJSON v@(Object _) = CabalConfig <$> parseJSON v
  parseJSON Null         = pure $ CabalConfig NoComponent
  parseJSON v            = typeMismatch "CabalConfig" v

data CabalComponent
  = CabalComponent { cabalPath      :: FilePath
                   , cabalComponent :: String
                   }

instance FromJSON CabalComponent where
  parseJSON =
    let parseCabalComponent obj = CabalComponent
                                    <$> obj .: "path"
                                    <*> obj .: "component"
     in withObject "CabalComponent" parseCabalComponent

data StackConfig
  = StackConfig { stackYaml       :: Maybe FilePath
                , stackComponents :: OneOrManyComponents StackComponent
                }

data StackComponent
  = StackComponent { stackPath          :: FilePath
                   , stackComponent     :: String
                   , stackComponentYAML :: Maybe String
                   }

instance FromJSON StackConfig where
  parseJSON v@(Array _)     = StackConfig Nothing . ManyComponents <$> parseJSON v
  parseJSON v@(Object obj)  = StackConfig
                                <$> obj .:? "stackYaml"
                                <*> parseJSON v
  parseJSON Null            = pure $ StackConfig Nothing NoComponent
  parseJSON v               = typeMismatch "StackConfig" v

instance FromJSON StackComponent where
  parseJSON =
    let parseStackComponent obj = StackComponent
                                    <$> obj .: "path"
                                    <*> obj .: "component"
                                    <*> obj .:? "stackYaml"
     in withObject "StackComponent" parseStackComponent

data OneOrManyComponents component
  = SingleComponent String
  | ManyComponents [component]
  | NoComponent

instance FromJSON component => FromJSON (OneOrManyComponents component) where
  parseJSON =
    let parseStackComponents o = parseSingleComponent o <|> parseSubComponents o <|> pure NoComponent
        parseSingleComponent o = SingleComponent <$> o .: "component"
        parseSubComponents   o = ManyComponents <$> o .: "components"
     in withObject "StackComponents" parseStackComponents

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
   in BiosConfig <$> parseCallable obj
                 <*> parseDepsCallable obj
                 <*> (obj .:? "with-ghc")
