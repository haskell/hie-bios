{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HIE.Bios.Process
  ( CreateProcess(..)
  -- * Run processes with extra environment variables
  , readProcessWithCwd
  , readProcessWithCwd_
  , readProcessWithCwd'
  , readProcessWithOutputs
  , getCleanEnvironment
  -- * Find file utilities
  , findFileUpwards
  , findFileUpwardsPredicate
  , findFile
  )
  where

import Control.Applicative (optional)
import Control.DeepSeq
import Control.Exception (handleJust)
import System.Exit
import System.Directory hiding (findFile)
import Colog.Core (LogAction (..), WithSeverity (..), Severity (..), (<&))
import Control.Monad
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Data.Conduit.Process
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit as C
import qualified Data.Conduit.Text as C
import qualified Data.HashMap.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.Environment
import System.FilePath
import System.IO (hClose, hGetContents, hSetBuffering, BufferMode(LineBuffering), withFile, IOMode(..))
import System.IO.Error (isPermissionError)
import System.IO.Temp

import HIE.Bios.Types

-- | Wrapper around 'readCreateProcess' that sets the working directory and
-- clears the environment, suitable for invoking cabal/stack and raw ghc commands.
readProcessWithCwd :: LogAction IO (WithSeverity Log) -> FilePath -> FilePath -> [String] -> String -> IO (CradleLoadResult String)
readProcessWithCwd l dir cmd args stdin = runCradleResultT $ readProcessWithCwd_ l dir cmd args stdin

readProcessWithCwd_ :: LogAction IO (WithSeverity Log) -> FilePath -> FilePath -> [String] -> String -> CradleLoadResultT IO String
readProcessWithCwd_ l dir cmd args stdin = do
  cleanEnv <- liftIO getCleanEnvironment
  let createdProc' = (proc cmd args) { cwd = Just dir, env = Just cleanEnv }
  readProcessWithCwd' l createdProc' stdin

-- | Wrapper around 'readCreateProcessWithExitCode', wrapping the result in
-- a 'CradleLoadResult'. Provides better error messages than raw 'readCreateProcess'.
readProcessWithCwd' :: LogAction IO (WithSeverity Log) -> CreateProcess -> String -> CradleLoadResultT IO String
readProcessWithCwd' l createdProcess stdin = do
  mResult <- liftIO $ optional $ readCreateProcessWithExitCode createdProcess stdin
  liftIO $ l <& LogCreateProcessRun createdProcess `WithSeverity` Debug
  let cmdString = prettyCmdSpec $ cmdspec createdProcess
  case mResult of
    Just (ExitSuccess, stdo, _) -> pure stdo
    Just (exitCode, stdo, stde) -> throwCE $
      CradleError [] exitCode
        (["Error when calling " <> cmdString, stdo, stde] <> prettyProcessEnv createdProcess)
        []
    Nothing -> throwCE $
      CradleError [] ExitSuccess
        (["Couldn't execute " <> cmdString] <> prettyProcessEnv createdProcess)
        []


-- | Some environments (e.g. stack exec) include GHC_PACKAGE_PATH.
-- Cabal v2 *will* complain, even though or precisely because it ignores them.
-- Unset them from the environment to sidestep this
getCleanEnvironment :: IO [(String, String)]
getCleanEnvironment = do
  Map.toList . Map.delete "GHC_PACKAGE_PATH" . Map.fromList <$> getEnvironment

type Outputs = [OutputName]
type OutputName = String

-- | Call a given process with temp files for the process to write to.
-- * The process can discover the temp files paths by reading the environment.
-- * The contents of the temp files are returned by this function, if any.
-- * The logging function is called every time the process emits anything to stdout or stderr.
-- it can be used to report progress of the process to a user.
-- * The process is executed in the given directory.
readProcessWithOutputs
  :: Outputs  -- ^ Names of the outputs produced by this process
  -> LogAction IO (WithSeverity Log) -- ^ Output of the process is emitted as logs.
  -> FilePath -- ^ Working directory. Process is executed in this directory.
  -> CreateProcess -- ^ Parameters for the process to be executed.
  -> IO (ExitCode, [String], [String], [(OutputName, Maybe [String])])
readProcessWithOutputs outputNames l workDir cp = flip runContT return $ do
  old_env <- liftIO getCleanEnvironment
  output_files <- traverse (withOutput old_env) outputNames

  let process = cp { env = Just $ output_files ++ fromMaybe old_env (env cp),
                     cwd = Just workDir
                    }

    -- Windows line endings are not converted so you have to filter out `'r` characters
  let loggingConduit = C.decodeUtf8  C..| C.lines C..| C.filterE (/= '\r')
        C..| C.map T.unpack C..| C.iterM (\msg -> l <& LogProcessOutput msg `WithSeverity` Debug) C..| C.sinkList
  liftIO $ l <& LogCreateProcessRun process `WithSeverity` Info
  (ex, stdo, stde) <- liftIO $ sourceProcessWithStreams process mempty loggingConduit loggingConduit

  res <- forM output_files $ \(name,path) ->
          liftIO $ (name,) <$> readOutput path

  return (ex, stdo, stde, res)

    where
      readOutput :: FilePath -> IO (Maybe [String])
      readOutput path = do
        haveFile <- doesFileExist path
        if haveFile
          then withFile path ReadMode $ \handle -> do
            hSetBuffering handle LineBuffering
            !res <- force <$> hGetContents handle
            return $ Just $ lines $ filter (/= '\r') res
          else
            return Nothing

      withOutput :: [(String,String)] -> OutputName -> ContT a IO (OutputName, String)
      withOutput env' name =
        case lookup name env' of
          Just file@(_:_) -> ContT $ \action -> do
            removeFileIfExists file
            action (name, file)
          _ -> ContT $ \action -> withSystemTempFile name $ \ file h -> do
            hClose h
            removeFileIfExists file
            action (name, file)

------------------------------------------------------------------------------
-- Utilities


-- | Searches upwards for the first directory containing a file.
findFileUpwards :: FilePath -> FilePath -> MaybeT IO FilePath
findFileUpwards filename dir = do
  cnts <-
    liftIO
    $ handleJust
        -- Catch permission errors
        (\(e :: IOError) -> if isPermissionError e then Just False else Nothing)
        pure
        (doesFileExist (dir </> filename))
  case cnts of
    False | dir' == dir -> fail "No cabal files"
            | otherwise   -> findFileUpwards filename dir'
    True -> return dir
  where dir' = takeDirectory dir

-- | Searches upwards for the first directory containing a file to match
-- the predicate.
--
-- *WARNING*, this scans all the files of all the directories upward. If
-- appliable, prefer to use 'findFileUpwards'
findFileUpwardsPredicate :: (FilePath -> Bool) -> FilePath -> MaybeT IO FilePath
findFileUpwardsPredicate p dir = do
  cnts <-
    liftIO
    $ handleJust
        -- Catch permission errors
        (\(e :: IOError) -> if isPermissionError e then Just [] else Nothing)
        pure
        (findFile p dir)

  case cnts of
    [] | dir' == dir -> fail "No cabal files"
            | otherwise   -> findFileUpwardsPredicate p dir'
    _ : _ -> return dir
  where dir' = takeDirectory dir

-- | Sees if any file in the directory matches the predicate
findFile :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
findFile p dir = do
  b <- doesDirectoryExist dir
  if b then getFiles >>= filterM doesPredFileExist else return []
  where
    getFiles = filter p <$> getDirectoryContents dir
    doesPredFileExist file = doesFileExist $ dir </> file

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists f = do
  yes <- doesFileExist f
  when yes (removeFile f)

