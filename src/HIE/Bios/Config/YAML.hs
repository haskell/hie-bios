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
  , ConfigWarning(..)
  , configWarnings
  ) where

import           Control.Applicative ((<|>))
import           Data.Aeson
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key      as K
import           Data.Aeson.KeyMap   (lookup)
import qualified Data.Aeson.KeyMap   as KM
#else
import           Data.HashMap.Strict (lookup)
import qualified Data.HashMap.Strict as Map
import qualified Data.Text           as T
#endif
import           Data.Aeson.Types    (Parser, typeMismatch)
import qualified Data.Char           as C (toLower)
import qualified Data.Foldable       as F
import           Data.List           ((\\))
import           GHC.Generics        (Generic)
import           Prelude             hiding (lookup)

-- | The fields of an object, with the keys as 'String's.
objectFields :: Object -> [(String, Value)]
#if MIN_VERSION_aeson(2,0,0)
objectFields = map (\(k, v) -> (K.toString k, v)) . KM.toList
#else
objectFields = map (\(k, v) -> (T.unpack k, v)) . Map.toList
#endif

-- | Something in a @hie.yaml@ file that hie-bios doesn't recognise. It is
-- ignored rather than rejected, so that a file written for a newer hie-bios
-- can still be used by an older one.
data ConfigWarning
  = UnknownKeys
      { unknownKeysIn :: [String]
      -- ^ Where the keys were found, e.g. @["cradle", "cabal"]@.
      , unknownKeys   :: [String]
      , knownKeys     :: [String]
      }
  deriving (Show, Eq)

-- The keys each part of the configuration accepts.
cabalConfigKeys, cabalComponentKeys, stackConfigKeys, stackComponentKeys, biosConfigKeys :: [String]
cabalConfigKeys    = ["cabalProject", "component", "components", "componentsToLoad"]
cabalComponentKeys = ["path", "component", "cabalProject"]
stackConfigKeys    = ["component", "components", "stackYaml", "componentsToLoad"]
stackComponentKeys = ["path", "component", "stackYaml"]
biosConfigKeys     = ["program", "shell", "dependency-program", "dependency-shell", "with-ghc"]

-- | Find the keys in the @cradle@ section of a @hie.yaml@ file, given as an
-- unparsed 'Value', that hie-bios doesn't know about.
configWarnings :: Value -> [ConfigWarning]
configWarnings = inConfig []
  where
    inConfig at (Object o) = maybe [] (inCradle (at ++ ["cradle"])) (lookup "cradle" o)
    inConfig _ _ = []

    inCradle at (Object o) = concat
      [ case (key, value) of
          ("cabal", _) -> inSection at' cabalConfigKeys cabalComponentKeys value
          ("stack", _) -> inSection at' stackConfigKeys stackComponentKeys value
          ("bios", Object bios) -> unknownIn at' biosConfigKeys bios
          ("multi", Array subs) ->
            concat [ inMulti (at' ++ [show i]) sub | (i, sub) <- zip [0 :: Int ..] (F.toList subs) ]
          _ -> []
      | (key, value) <- objectFields o
      , let at' = at ++ [key]
      ]
    inCradle _ _ = []

    -- cabal and stack sections are either an object, possibly with a list of
    -- components, or just the list of components.
    inSection at sectionKeys componentKeys value = case value of
      Object o -> unknownIn at sectionKeys o
                    ++ maybe [] (inComponents (at ++ ["components"]) componentKeys) (lookup "components" o)
      Array _  -> inComponents at componentKeys value
      _        -> []

    inComponents at componentKeys (Array cs) = concat
      [ case c of
          Object o -> unknownIn (at ++ [show i]) componentKeys o
          _        -> []
      | (i, c) <- zip [0 :: Int ..] (F.toList cs)
      ]
    inComponents _ _ _ = []

    -- each entry of a multi cradle has its own nested configuration
    inMulti at (Object o) = maybe [] (inConfig (at ++ ["config"])) (lookup "config" o)
    inMulti _ _ = []

    unknownIn at allowed o = case map fst (objectFields o) \\ allowed of
      []    -> []
      extra -> [UnknownKeys at extra allowed]

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
                , cabalComponentsToLoad :: Maybe [String]
                , cabalComponents :: OneOrManyComponents CabalComponent
                }

instance FromJSON CabalConfig where
  parseJSON v@(Array _)     = CabalConfig Nothing Nothing . ManyComponents <$> parseJSON v
  parseJSON v@(Object obj)  = CabalConfig
                                  <$> obj .:? "cabalProject"
                                  <*> obj .:? "componentsToLoad"
                                  <*> parseJSON v
  parseJSON Null            = pure $ CabalConfig Nothing Nothing NoComponent
  parseJSON v               = typeMismatch "CabalConfig" v

data CabalComponent
  = CabalComponent { cabalPath      :: FilePath
                   , cabalComponent :: String
                   , cabalComponentProject :: Maybe FilePath
                   }

instance FromJSON CabalComponent where
  parseJSON =
    let parseCabalComponent obj = CabalComponent
                                    <$> obj .: "path"
                                    <*> obj .: "component"
                                    <*> obj .:? "cabalProject"
     in withObject "CabalComponent" parseCabalComponent

data StackConfig
  = StackConfig { stackYaml       :: Maybe FilePath
                , stackComponentsToLoad :: Maybe [String]
                , stackComponents :: OneOrManyComponents StackComponent
                }

data StackComponent
  = StackComponent { stackPath          :: FilePath
                   , stackComponent     :: String
                   , stackComponentYAML :: Maybe FilePath
                   }

instance FromJSON StackConfig where
  parseJSON v@(Array _)     = StackConfig Nothing Nothing . ManyComponents <$> parseJSON v
  parseJSON v@(Object obj)  = StackConfig
                                  <$> obj .:? "stackYaml"
                                  <*> obj .:? "componentsToLoad"
                                  <*> parseJSON v
  parseJSON Null            = pure $ StackConfig Nothing Nothing NoComponent
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

instance FromJSON a => FromJSON (OneOrManyComponents a) where
  parseJSON (Object obj)
    | Just v <- lookup "component"  obj = SingleComponent <$> parseJSON v
    | Just v <- lookup "components" obj = ManyComponents <$> parseJSON v
    | otherwise                         = pure NoComponent
  parseJSON Null = pure NoComponent
  parseJSON v    = typeMismatch "OneOrManyComponents" v

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
