{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HIE.Bios.Types where

import           System.Exit
import           Control.Exception              ( Exception )

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
  } deriving (Show, Functor)

type LoggingFunction = String -> IO ()

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

data CradleAction a = CradleAction
  { actionName    :: ActionName a
  -- ^ Name of the action.
  , runCradle     :: LoggingFunction -> FilePath -> IO (CradleLoadResult LoadResult)
  -- ^ Options to compile the given file with.
  --
  -- The given FilePath /must/ be part of 'LoadResult.loadResultComponent' if
  -- the loading operation succeeds.
  , runGhcCmd :: [String] -> IO (CradleLoadResult String)
  -- ^ Executes the @ghc@ binary that is usually used to
  -- build the cradle. E.g. for a cabal cradle this should be
  -- equivalent to @cabal exec ghc -- args@
  }
  deriving (Functor)

instance Show a => Show (CradleAction a) where
  show CradleAction { actionName = name } = "CradleAction: " ++ show name

type LoadResult = LoadResult' ComponentOptions

-- | Record for expressing successful loading.
-- Can express partial success.
data LoadResult' a = LoadResult
  { loadResultComponent :: Maybe a
  -- ^ Component options for the FilePath that produced this 'LoadResult'.
  -- See 'CradleAction.runCradle' for information on how to produce a 'LoadResult'.
  --
  -- This field can be 'Nothing' to indicate that loading partially failed/succeeded.
  , loadResultDependencies :: [a]
  -- ^ Direct or indirect dependencies from the component from above.
  -- Indirect means that it is not required that 'ComponentOptions' in this list
  -- are required dependencies of 'loadResultComponent'. It is specifically allowed
  -- to list 'ComponentOptions' that have no relation with 'loadResultComponent'.
  --
  -- Example:
  --
  -- Assume we load an executable component, then its options must be located
  -- in 'loadResultComponent' and its local dependencies in 'loadResultDependencies',
  -- but additionally it is possible to list other executable component's
  -- options in 'loadResultDependencies'.
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Create a simple LoadResult from a single 'ComponentOptions' record.
-- Sets the 'ComponentOptions' as 'loadResultComponent'.
mkSimpleLoadResult :: ComponentOptions -> LoadResult
mkSimpleLoadResult opts = LoadResult
  { loadResultComponent = Just opts
  , loadResultDependencies = []
  }

-- | Helper to access the main component if there is one.
pattern Main :: a -> LoadResult' a
pattern Main opts <- (loadResultComponent -> Just opts)

-- | Result of an attempt to set up a GHC session for a 'Cradle'.
-- This is the go-to error handling mechanism. When possible, this
-- should be preferred over throwing exceptions.
data CradleLoadResult r
  = CradleSuccess r -- ^ The cradle succeeded and returned these options.
  | CradleFail CradleError -- ^ We tried to load the cradle and it failed.
  | CradleNone -- ^ No attempt was made to load the cradle.
 deriving (Functor, Foldable, Traversable, Show, Eq)


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

bindIO :: CradleLoadResult a -> (a -> IO (CradleLoadResult b)) -> IO (CradleLoadResult b)
bindIO  (CradleSuccess r) k = k r
bindIO (CradleFail err) _ = return $ CradleFail err
bindIO CradleNone _ = return CradleNone


data CradleError = CradleError
  { cradleErrorDependencies :: [FilePath]
  -- ^ Dependencies of the cradle that failed to load.
  -- Can be watched for changes to attempt a reload of the cradle.
  , cradleErrorExitCode :: ExitCode
  -- ^ ExitCode of the cradle loading mechanism.
  , cradleErrorStderr :: [String]
  -- ^ Standard error output that can be shown to users to explain
  -- the loading error.
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
