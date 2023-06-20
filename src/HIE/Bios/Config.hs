{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Logic and datatypes for parsing @hie.yaml@ files.
module HIE.Bios.Config(
    readConfig,
    Config(..),
    CradleConfig(..),
    CabalType,
    pattern CabalType,
    cabalComponent,
    cabalProjectFile,
    StackType,
    pattern StackType,
    stackComponent,
    stackYaml,
    CradleTree(..),
    Callable(..)
    ) where

import Control.Exception
import           Data.Maybe (mapMaybe, fromMaybe)
import           Data.Monoid (Last(..))
import           Data.Aeson (JSONPath)
import           Data.Yaml
import           Data.Yaml.Internal (Warning(..))

import           HIE.Bios.Config.YAML (CradleConfigYAML)
import qualified HIE.Bios.Config.YAML as YAML


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
        , cradleTree :: CradleTree a
        -- ^ Type of the cradle to use. Actions to obtain
        -- compiler flags from are dependant on this field.
        }
        deriving (Show, Eq, Functor)

data Callable = Program FilePath | Command String
    deriving (Show, Eq)

-- | A cabal yaml configuration consists of component configuration and project configuration.
--
-- The former specifies how we can find the compilation flags for any filepath
-- in the project.
-- There might be an explicit mapping from source directories to components,
-- or we let cabal figure it out on its own.
--
-- Project configuration is the 'cabal.project' file, we is by default named
-- 'cabal.project'. We allow to override that name to have an HLS specific
-- project configuration file.
data CabalType
    = CabalType_ { _cabalComponent :: !(Last String), _cabalProjectFile :: !(Last FilePath) }
    deriving (Eq)

instance Semigroup CabalType where
    CabalType_ cr cpr <> CabalType_ cl cpl = CabalType_ (cr <> cl) (cpr <> cpl)

instance Monoid CabalType where
    mempty = CabalType_ mempty mempty

pattern CabalType :: Maybe String -> Maybe FilePath -> CabalType
pattern CabalType { cabalComponent, cabalProjectFile } = CabalType_ (Last cabalComponent) (Last cabalProjectFile)
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

pattern StackType :: Maybe String -> Maybe FilePath -> StackType
pattern StackType { stackComponent, stackYaml } = StackType_ (Last stackComponent) (Last stackYaml)
{-# COMPLETE StackType #-}

instance Show StackType where
  show = show . Stack

data CradleTree a
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

instance Show (CradleTree a) where
    show (Cabal comp) = "Cabal {component = " ++ show (cabalComponent comp) ++ "}"
    show (CabalMulti d a) = "CabalMulti {defaultCabal = " ++ show d ++ ", subCabalComponents = " ++ show a ++ "}"
    show (Stack comp) = "Stack {component = " ++ show (stackComponent comp) ++ ", stackYaml = " ++ show (stackYaml comp) ++ "}"
    show (StackMulti d a) = "StackMulti {defaultStack = " ++ show d ++ ", subStackComponents = "  ++ show a ++ "}"
    show Bios { call, depsCall } = "Bios {call = " ++ show call ++ ", depsCall = " ++ show depsCall ++ "}"
    show (Direct args) = "Direct {arguments = " ++ show args ++ "}"
    show None = "None"
    show (Multi a) = "Multi " ++ show a
    show (Other _ val) = "Other {originalYamlValue = " ++ show val ++ "}"

readConfig :: FromJSON a => FilePath -> IO (Config a)
readConfig fp = do
  result <- decodeFileWithWarnings fp
  fmap fromYAMLConfig $ either throwIO failOnAnyDuplicate result
  where
    failOnAnyDuplicate :: ([Warning], CradleConfigYAML a) -> IO (CradleConfigYAML a)
    failOnAnyDuplicate (warnings, config) = do
        _ <- case mapMaybe failOnDuplicate warnings of
                dups@(_:_) -> throwIO $ InvalidYaml $ Just $ YamlException
                                      $ "Duplicate keys are not allowed, found: " ++ show dups
                _ -> return ()
        return config
    -- future proofing in case more warnings are added
    failOnDuplicate :: Warning -> Maybe JSONPath
    failOnDuplicate (DuplicateKey a) = Just a

fromYAMLConfig :: CradleConfigYAML a -> Config a
fromYAMLConfig cradleYAML = Config $ CradleConfig (fromMaybe [] $ YAML.dependencies cradleYAML)
                                                  (toCradleTree $ YAML.cradle cradleYAML)

toCradleTree :: YAML.CradleComponent a -> CradleTree a
toCradleTree (YAML.Multi cpts)  =
  Multi $ (\(YAML.MultiSubComponent fp' cfg) -> (fp', cradle $ fromYAMLConfig cfg)) <$> cpts
toCradleTree (YAML.Stack (YAML.StackConfig yaml cpts)) =
  case cpts of
    YAML.NoComponent          -> Stack $ StackType Nothing yaml
    (YAML.SingleComponent c)  -> Stack $ StackType (Just c) yaml
    (YAML.ManyComponents cs)  -> StackMulti (StackType Nothing yaml)
                                            ((\(YAML.StackComponent fp' c cYAML) ->
                                              (fp', StackType (Just c) cYAML)) <$> cs)
toCradleTree (YAML.Cabal (YAML.CabalConfig prjFile cpts)) =
  case cpts of
    YAML.NoComponent          -> Cabal $ CabalType Nothing prjFile
    (YAML.SingleComponent c)  -> Cabal $ CabalType (Just c) prjFile
    (YAML.ManyComponents cs)  -> CabalMulti (CabalType Nothing prjFile)
                                            ((\(YAML.CabalComponent fp' c cPrjFile) ->
                                              (fp', CabalType (Just c) cPrjFile)) <$> cs)
toCradleTree (YAML.Direct cfg)  = Direct (YAML.arguments cfg)
toCradleTree (YAML.Bios cfg)    = Bios  (toCallable $ YAML.callable cfg)
                                        (toCallable <$> YAML.depsCallable cfg)
                                        (YAML.ghcPath cfg)
toCradleTree (YAML.None _)      = None
toCradleTree (YAML.Other cfg)   = Other (YAML.otherConfig cfg)
                                        (YAML.originalYamlValue cfg)

toCallable :: YAML.Callable -> Callable
toCallable (YAML.Program p) = Program p
toCallable (YAML.Shell c)   = Command c
