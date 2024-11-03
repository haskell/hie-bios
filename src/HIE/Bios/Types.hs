{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
module HIE.Bios.Types where

import           System.Exit
import qualified Colog.Core as L
import           Control.Exception              ( Exception )
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail as Fail
#endif
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Prettyprinter
import           System.Process.Extra (CreateProcess (env, cmdspec), CmdSpec (..))

----------------------------------------------------------------
-- Environment variables used by hie-bios.
--
-- If you need more, add a constant here.
----------------------------------------------------------------

-- | Environment variable containing the filepath to which
-- cradle actions write their results to.
-- If the filepath does not exist, cradle actions must create them.
hie_bios_output :: String
hie_bios_output = "HIE_BIOS_OUTPUT"

-- | Environment variable pointing to the GHC location used by
-- cabal's and stack's GHC wrapper.
--
-- If not set, will default to sensible defaults.
hie_bios_ghc :: String
hie_bios_ghc = "HIE_BIOS_GHC"

-- | Environment variable with extra arguments passed to the GHC location
-- in cabal's and stack's GHC wrapper.
--
-- If not set, assume no extra arguments.
hie_bios_ghc_args :: String
hie_bios_ghc_args = "HIE_BIOS_GHC_ARGS"

-- | Environment variable pointing to the source file location that caused
-- the cradle action to be executed.
hie_bios_arg :: String
hie_bios_arg = "HIE_BIOS_ARG"

-- | Environment variable pointing to a filepath to which dependencies
-- of a cradle can be written to by the cradle action.
hie_bios_deps :: String
hie_bios_deps = "HIE_BIOS_DEPS"

----------------------------------------------------------------

-- | The environment of a single 'Cradle'.
-- A 'Cradle' is a unit for the respective build-system.
--
-- It contains the root directory of the 'Cradle', the name of
-- the 'Cradle' (for debugging purposes), and knows how to set up
-- a GHC session that is able to compile files that are part of this 'Cradle'.
--
-- A 'Cradle' may be a single unit in the \"cabal-install\" context, or
-- the whole package, comparable to how \"stack\" works.
data Cradle a = Cradle {
  -- | The project root directory.
    cradleRootDir    :: FilePath
  -- | The action which needs to be executed to get the correct
  -- command line arguments.
  , cradleOptsProg   :: CradleAction a
  , cradleLogger  :: L.LogAction IO (L.WithSeverity Log)
  } deriving (Functor)

instance Show a => Show (Cradle a) where
  show (Cradle root prog _)
    = "Cradle{ cradleRootDir = " ++ show root
          ++", cradleOptsProg = " ++ show prog
          ++"}"

data ActionName a
  = Stack
  | Cabal
  | Bios
  | Default
  | Multi
  | Direct
  | None
  | Other a
  deriving (Show, Eq, Ord, Functor)

data Log
  = LogAny !T.Text
  | LogProcessOutput String
  | LogCreateProcessRun CreateProcess
  | LogProcessRun FilePath [FilePath]
  | LogRequestedCradleLoadStyle !T.Text !LoadStyle
  | LogComputedCradleLoadStyle !T.Text !LoadStyle
  | LogLoadWithContextUnsupported !T.Text !(Maybe T.Text)
  deriving (Show)

instance Pretty Log where
  pretty (LogAny s) = pretty s
  pretty (LogProcessOutput s) = pretty s
  pretty (LogProcessRun fp args) = pretty fp <+> pretty (unwords args)
  pretty (LogCreateProcessRun cp) =
    vcat $
      [ case cmdspec cp of
          ShellCommand sh -> pretty sh
          RawCommand cmd args -> pretty cmd <+> pretty (unwords args)
      ]
      <>
      if null envText
        then []
        else
          [ indent 2 $ "Environment Variables"
          , indent 2 $ vcat envText
          ]
    where
      envText = map (indent 2 . pretty) $ prettyProcessEnv cp
  pretty (LogRequestedCradleLoadStyle crdlName ls) =
    "Requested to load" <+> pretty crdlName <+> "cradle" <+> case ls of
      LoadFile -> "using single file mode"
      LoadWithContext fps -> "using all files (multi-components):" <> line <> indent 4 (pretty fps)
  pretty (LogComputedCradleLoadStyle crdlName ls) =
    "Load" <+> pretty crdlName <+> "cradle" <+> case ls of
      LoadFile -> "using single file"
      LoadWithContext _ -> "using all files (multi-components)"

  pretty (LogLoadWithContextUnsupported crdlName mReason) =
    pretty crdlName <+> "cradle doesn't support loading using all files (multi-components)" <>
      case mReason of
        Nothing -> "."
        Just reason -> ", because:" <+> pretty reason <> "."
      <+> "Falling back loading to single file mode."

-- | The 'LoadStyle' instructs a cradle on how to load a given file target.
data LoadStyle
  = LoadFile
  -- ^ Instruct the cradle to load the given file target.
  --
  -- What this entails depends on the cradle. For example, the 'cabal' cradle
  -- will configure the whole component the file target belongs to, and produce
  -- component options to load the component, which is the minimal unit of code in cabal repl.
  -- A 'default' cradle, on the other hand, will only load the given filepath.
  | LoadWithContext [FilePath]
  -- ^ Give a cradle additional context for loading a file target.
  --
  -- The context instructs the cradle to load the file target, while also loading
  -- the given filepaths.
  -- This is useful for cradles that support loading multiple code units at once,
  -- e.g. cabal cradles can use the 'multi-repl' feature to set up a multiple home unit
  -- session in GHC.
  deriving (Show, Eq, Ord)

data CradleAction a = CradleAction {
                        actionName    :: ActionName a
                      -- ^ Name of the action.
                      , runCradle     :: FilePath -> LoadStyle -> IO (CradleLoadResult ComponentOptions)
                      -- ^ Options to compile the given file with.
                      , runGhcCmd     :: [String] -> IO (CradleLoadResult String)
                      -- ^ Executes the @ghc@ binary that is usually used to
                      -- build the cradle. E.g. for a cabal cradle this should be
                      -- equivalent to @cabal exec ghc -- args@
                      }
  deriving (Functor)

instance Show a => Show (CradleAction a) where
  show CradleAction { actionName = name } = "CradleAction: " ++ show name

-- | Result of an attempt to set up a GHC session for a 'Cradle'.
-- This is the go-to error handling mechanism. When possible, this
-- should be preferred over throwing exceptions.
data CradleLoadResult r
  = CradleSuccess r -- ^ The cradle succeeded and returned these options.
  | CradleFail CradleError -- ^ We tried to load the cradle and it failed.
  | CradleNone -- ^ No attempt was made to load the cradle.
 deriving (Functor, Foldable, Traversable, Show, Eq)

cradleLoadResult :: c -> (CradleError -> c) -> (r -> c) -> CradleLoadResult r -> c
cradleLoadResult c _ _ CradleNone        = c
cradleLoadResult _ f _ (CradleFail e)    = f e
cradleLoadResult _ _ f (CradleSuccess r) = f r

instance Applicative CradleLoadResult where
  pure = CradleSuccess
  CradleSuccess a <*> CradleSuccess b = CradleSuccess (a b)
  CradleFail err <*> _ = CradleFail err
  _ <*> CradleFail err = CradleFail err
  _ <*> _ = CradleNone

instance Monad CradleLoadResult where
  return = pure
  CradleSuccess r >>= k = k r
  CradleFail err >>= _ = CradleFail err
  CradleNone >>= _ = CradleNone

newtype CradleLoadResultT m a = CradleLoadResultT { runCradleResultT :: m (CradleLoadResult a) }

instance Functor f => Functor (CradleLoadResultT f) where
  {-# INLINE fmap #-}
  fmap f = CradleLoadResultT . (fmap . fmap) f . runCradleResultT

instance (Monad m, Applicative m) => Applicative (CradleLoadResultT m) where
  {-# INLINE pure #-}
  pure = CradleLoadResultT . pure . CradleSuccess
  {-# INLINE (<*>) #-}
  x <*> f = CradleLoadResultT $ do
              a <- runCradleResultT x
              case a of
                CradleSuccess a' -> do
                  b <- runCradleResultT f
                  case b of
                    CradleSuccess b' -> pure (CradleSuccess (a' b'))
                    CradleFail err -> pure $ CradleFail err
                    CradleNone -> pure CradleNone
                CradleFail err -> pure $ CradleFail err
                CradleNone -> pure CradleNone

instance Monad m => Monad (CradleLoadResultT m) where
  {-# INLINE return #-}
  return = pure
  {-# INLINE (>>=) #-}
  x >>= f = CradleLoadResultT $ do
    val <- runCradleResultT x
    case val of
      CradleSuccess r -> runCradleResultT . f $ r
      CradleFail err -> return $ CradleFail err
      CradleNone -> return $ CradleNone
#if !(MIN_VERSION_base(4,13,0))
  fail = CradleLoadResultT . fail
  {-# INLINE fail #-}
#endif
#if MIN_VERSION_base(4,9,0)
instance Fail.MonadFail m => Fail.MonadFail (CradleLoadResultT m) where
  fail = CradleLoadResultT . Fail.fail
  {-# INLINE fail #-}
#endif

instance MonadTrans CradleLoadResultT where
    lift = CradleLoadResultT . fmap CradleSuccess
    {-# INLINE lift #-}

instance (MonadIO m) => MonadIO (CradleLoadResultT m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

modCradleError :: Monad m => CradleLoadResultT m a -> (CradleError -> m CradleError) -> CradleLoadResultT m a
modCradleError action f = CradleLoadResultT $ do
  a <- runCradleResultT action
  case a of
    CradleFail err -> CradleFail <$> f err
    _ -> pure a

throwCE :: Monad m => CradleError -> CradleLoadResultT m a
throwCE = CradleLoadResultT . return . CradleFail

data CradleError = CradleError
  { cradleErrorDependencies :: [FilePath]
  -- ^ Dependencies of the cradle that failed to load.
  -- Can be watched for changes to attempt a reload of the cradle.
  , cradleErrorExitCode :: ExitCode
  -- ^ ExitCode of the cradle loading mechanism.
  , cradleErrorStderr :: [String]
  -- ^ Standard error output that can be shown to users to explain
  -- the loading error.
  , cradleErrorLoadingFiles :: [FilePath]
  }
  deriving (Show, Eq)

instance Exception CradleError where
----------------------------------------------------------------

-- | Option information for GHC
data ComponentOptions = ComponentOptions {
    componentOptions  :: [String]  -- ^ Command line options.
  , componentRoot :: FilePath
  -- ^ Root directory of the component. All 'componentOptions' are either
  -- absolute, or relative to this directory.
  , componentDependencies :: [FilePath]
  -- ^ Dependencies of a cradle that might change the cradle.
  -- Contains both files specified in hie.yaml as well as
  -- specified by the build-tool if there is any.
  -- FilePaths are expected to be relative to the `cradleRootDir`
  -- to which this CradleAction belongs to.
  -- Files returned by this action might not actually exist.
  -- This is useful, because, sometimes, adding specific files
  -- changes the options that a Cradle may return, thus, needs reload
  -- as soon as these files are created.
  } deriving (Eq, Ord, Show)


-- ------------------------------------------------

-- | Prettify 'CmdSpec', so we can show the command to a user
prettyCmdSpec :: CmdSpec -> String
prettyCmdSpec (ShellCommand s) = s
prettyCmdSpec (RawCommand cmd args) = cmd ++ " " ++ unwords args

-- | Pretty print hie-bios's relevant environment variables.
prettyProcessEnv :: CreateProcess -> [String]
prettyProcessEnv p =
  [ key <> ": " <> value
  | (key, value) <- fromMaybe [] (env p)
  , key `elem` [ hie_bios_output
               , hie_bios_ghc
               , hie_bios_ghc_args
               , hie_bios_arg
               , hie_bios_deps
               ]
  ]
