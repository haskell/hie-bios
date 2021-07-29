{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad ( forM )
import Data.Version (showVersion)
import Options.Applicative
import System.Directory (getCurrentDirectory)
import System.IO (stdout, hSetEncoding, utf8)
import System.FilePath( (</>) )

import HIE.Bios
import HIE.Bios.Ghc.Check
import HIE.Bios.Ghc.Gap as Gap
import HIE.Bios.Internal.Debug
import Paths_hie_bios

----------------------------------------------------------------

progVersion :: String
progVersion = "hie-bios version " ++ showVersion version ++ " compiled by GHC " ++ Gap.ghcVersion ++ "\n"

data Command
  = Check { checkTargetFiles :: [FilePath] }
  | Flags { flagTargetFiles :: [FilePath] }
  | Debug { debugComponents :: FilePath }
  | ConfigInfo { configFiles :: [FilePath] }
  | CradleInfo { cradleFiles :: [FilePath] }
  | Root
  | Version


filepathParser :: Parser [FilePath]
filepathParser = some (argument str ( metavar "TARGET_FILES..."))

progInfo :: ParserInfo Command
progInfo = info (progParser <**> helper)
  ( fullDesc
  <> progDesc "hie-bios is the way to specify how haskell-language-server and ghcide set up a GHC API session.\
              \Delivers the full set of flags to pass to GHC in order to build the project."
  <> header progVersion
  <> footer "You can report issues/contribute at https://github.com/mpickering/hie-bios")

progParser :: Parser Command
progParser = subparser
    (command "check" (info (Check <$> filepathParser) (progDesc "Try to load modules into the GHC API."))
    <> command "flags" (info (Flags <$> filepathParser) (progDesc "Print out the options that hie-bios thinks you will need to load a file."))
    <> command "debug" (info (Debug <$> argument str ( metavar "TARGET_FILES...")) (progDesc "Print out the options that hie-bios thinks you will need to load a file."))
    <> command "config" (info (ConfigInfo <$> filepathParser) (progDesc "Print out the cradle config."))
    <> command "cradle" (info (CradleInfo <$> filepathParser) (progDesc "."))
    <> command "root" (info (pure Root) (progDesc "Display the path towards the selected hie.yaml."))
    <> command "version" (info (pure Version) (progDesc "Print version and exit."))
    )


----------------------------------------------------------------

main :: IO ()
main = do
    hSetEncoding stdout utf8
    cwd <- getCurrentDirectory
    cradle <-
        -- find cradle does a takeDirectory on the argument, so make it into a file
        findCradle (cwd </> "File.hs") >>= \case
          Just yaml -> loadCradle yaml
          Nothing -> loadImplicitCradle (cwd </> "File.hs")

    cmd <- execParser progInfo

    res <- case cmd of
      Check targetFiles -> checkSyntax cradle targetFiles
      Debug files -> case files of
        [] -> debugInfo (cradleRootDir cradle) cradle
        fp -> debugInfo fp cradle
      Flags files -> case files of
        -- TODO force optparse to acquire one
        [] -> error "too few arguments"
        _ -> do
          res <- forM files $ \fp -> do
                  res <- getCompilerOptions fp cradle
                  case res of
                      CradleFail (CradleError _deps _ex err) ->
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
      CradleInfo files -> cradleInfo files
      Root    -> rootInfo cradle
      Version -> return progVersion
    putStr res
