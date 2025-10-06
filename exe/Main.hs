{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad ( forM )
import qualified Colog.Core as L
import Data.Version (showVersion)
import Prettyprinter
import Options.Applicative
import System.Directory (getCurrentDirectory, makeAbsolute)
import System.IO (stdout, hSetEncoding, utf8)
import System.FilePath( (</>) )

import HIE.Bios
import HIE.Bios.Ghc.Check
import HIE.Bios.Ghc.Gap as Gap
import HIE.Bios.Internal.Debug
import HIE.Bios.Types (LoadStyle(..))
import Paths_hie_bios
import Data.Void (Void)
import Data.Function

----------------------------------------------------------------

progVersion :: String
progVersion = "hie-bios version " ++ showVersion version ++ " compiled by GHC " ++ Gap.ghcVersion ++ "\n"

data UseLoadStyle
  = UseSingleFile
  | UseMultiFile

data Cli = Cli
  { logLevel :: Maybe L.Severity
  , biosCommand :: Command
  }

data Command
  = Check { checkTargetFiles :: [FilePath] }
  | Flags { flagTargetFiles :: [FilePath] }
  | Debug { debugUseMultiLoadStyle :: UseLoadStyle, debugComponents :: [FilePath] }
  | ConfigInfo { configFiles :: [FilePath] }
  | CradleInfo { cradleFiles :: [FilePath] }
  | Root
  | Version


filepathParser :: Parser FilePath
filepathParser = argument str ( metavar "TARGET_FILES...")

progInfo :: ParserInfo Cli
progInfo = info (cliParser <**> helper)
  ( fullDesc
  <> progDesc "hie-bios is the way to specify how haskell-language-server and ghcide set up a GHC API session.\
              \Delivers the full set of flags to pass to GHC in order to build the project."
  <> header progVersion
  <> footer "You can report issues/contribute at https://github.com/mpickering/hie-bios")

cliParser :: Parser Cli
cliParser = Cli
  <$> optional sevParser
  <*> progParser

sevParser :: Parser L.Severity
sevParser =
  flag' L.Debug (short 'v')

progParser :: Parser Command
progParser = hsubparser
    (command "check" (info (Check <$> some filepathParser) (progDesc "Try to load modules into the GHC API."))
    <> command "flags" (info (Flags <$> some filepathParser) (progDesc "Print out the options that hie-bios thinks you will need to load a file."))
    <> command "debug" (info (Debug <$> loadStyleParser <*> many filepathParser) (progDesc "Print out the options that hie-bios thinks you will need to load a file."))
    <> command "config" (info (ConfigInfo <$> some filepathParser) (progDesc "Print out the cradle config location."))
    <> command "cradle" (info (CradleInfo <$> some filepathParser) (progDesc "Print out only the cradle type."))
    <> command "root" (info (pure Root) (progDesc "Display the path towards the selected hie.yaml."))
    <> command "version" (info (pure Version) (progDesc "Print version and exit."))
    )

loadStyleParser :: Parser UseLoadStyle
loadStyleParser =
  flag UseSingleFile UseMultiFile (long "multi" <> help "Load all targets in bulk if supported")

----------------------------------------------------------------

main :: IO ()
main = do
    hSetEncoding stdout utf8
    cwd <- getCurrentDirectory
    cli <- execParser progInfo
    let
      printLog (L.WithSeverity l sev) = "[" ++ show sev ++ "] " ++ show (pretty l)
      logger :: forall a . Pretty a => L.LogAction IO (L.WithSeverity a)
      logger = L.logStringStderr
        & L.cmap printLog
        & L.cfilter (\msg -> case logLevel cli of
            Nothing -> False
            Just lvl -> L.getSeverity msg >= lvl )

    cradle <-
        -- find cradle does a takeDirectory on the argument, so make it into a file
        findCradle (cwd </> "File.hs") >>= \case
          Just yaml -> loadCradle logger yaml
          Nothing -> loadImplicitCradle logger (cwd </> "File.hs")

    res <- case biosCommand cli of
      Check targetFiles -> checkSyntax logger cradle targetFiles
      Debug useMultiStyle files -> do
        absFiles <- traverse makeAbsolute files
        debugFiles absFiles useMultiStyle cradle
      Flags files -> case files of
        -- TODO force optparse to acquire one
        [] -> error "too few arguments"
        _ -> do
          res <- forM files $ \fp -> do
                  res <- getCompilerOptions fp LoadFile cradle
                  case res of
                      CradleFail (CradleError _deps _ex err _fps) ->
                        return $ "Failed to show flags for \""
                                                  ++ fp
                                                  ++ "\": " ++ show err
                      CradleSuccess opts ->
                        return $ unlines ["Options: " ++ show (componentOptions opts)
                                        ,"ComponentDir: " ++ componentRoot opts
                                        ,"Dependencies: " ++ show (componentDependencies opts) ]
                      CradleNone -> return $ "No flags/None Cradle: component " ++ fp ++ " should not be loaded"
          return (unlines res)
      ConfigInfo files -> configInfo files
      CradleInfo files -> cradleInfo logger files
      Root    -> rootInfo cradle
      Version -> return progVersion
    putStr res

debugFiles :: [FilePath] -> UseLoadStyle -> Cradle Void -> IO String
debugFiles fps useLoadStyle cradle = case useLoadStyle of
  UseSingleFile -> debugSingle
  UseMultiFile -> debugBulk
  where
    debugSingle = case fps of
      [] -> debugInfo (cradleRootDir cradle) LoadFile cradle
      _ -> concat <$> traverse (\fp -> debugInfo fp LoadFile cradle) fps

    debugBulk = case fps of
      [] -> debugInfo (cradleRootDir cradle) (LoadWithContext []) cradle
      fp:otherFps -> debugInfo fp (LoadWithContext otherFps) cradle
