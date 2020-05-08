{-# LANGUAGE DeriveDataTypeable, LambdaCase #-}

module Main where

import Config (cProjectVersion)

import Control.Exception (Exception, Handler(..), ErrorCall(..))
import qualified Control.Exception as E
import Control.Monad ( forM )
import Data.Typeable (Typeable)
import Data.Version (showVersion)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStr, hPutStrLn, stdout, stderr, hSetEncoding, utf8)
import System.FilePath( (</>) )

import HIE.Bios
import HIE.Bios.Ghc.Check
import HIE.Bios.Internal.Debug
import Paths_hie_bios

----------------------------------------------------------------

progVersion :: String
progVersion = "hie-bios version " ++ showVersion version ++ " compiled by GHC " ++ cProjectVersion ++ "\n"

ghcOptHelp :: String
ghcOptHelp = " [-g GHC_opt1 -g GHC_opt2 ...] "

usage :: String
usage =    progVersion
        ++ "Usage:\n"
        ++ "\t hie-bios check" ++ ghcOptHelp ++ "<HaskellFiles...>\n"
        ++ "\t hie-bios expand <HaskellFiles...>\n"
        ++ "\t hie-bios flags <HaskellFiles...>\n"
        ++ "\t hie-bios debug [<ComponentDir>]\n"
        ++ "\t hie-bios config <HaskellFiles...>\n"
        ++ "\t hie-bios cradle <HaskellFiles...>\n"
        ++ "\t hie-bios root\n"
        ++ "\t hie-bios version\n"

----------------------------------------------------------------

data HhpcError = SafeList
               | TooManyArguments String
               | NotEnoughArguments String
               | NoSuchCommand String
               | CmdArg [String]
               | FileNotExist String deriving (Show, Typeable)

instance Exception HhpcError

----------------------------------------------------------------

main :: IO ()
main = flip E.catches handlers $ do
    hSetEncoding stdout utf8
    args <- getArgs
    cwd <- getCurrentDirectory
    cradle <-
        -- find cradle does a takeDirectory on the argument, so make it into a file
        findCradle (cwd </> "File.hs") >>= \case
          Just yaml -> loadCradle yaml
          Nothing -> loadImplicitCradle (cwd </> "File.hs")
    let cmdArg0 = args !. 0
        remainingArgs = tail args
    res <- case cmdArg0 of
      "check"   -> checkSyntax cradle remainingArgs
      "debug"
        | null remainingArgs -> debugInfo (cradleRootDir cradle) cradle
        | (fp:_) <- remainingArgs -> debugInfo fp cradle
      "root"    -> rootInfo cradle
      "version" -> return progVersion
      "config" -> configInfo remainingArgs
      "cradle" -> cradleInfo remainingArgs
      "flags"
        | null remainingArgs -> E.throw $ NotEnoughArguments cmdArg0
        | otherwise -> do
        res <- forM remainingArgs $ \fp -> do
                res <- getCompilerOptions fp cradle
                case res of
                    CradleFail (CradleError _deps _ex err) ->
                      return $ "Failed to show flags for \""
                                                ++ fp
                                                ++ "\": " ++ show err
                    CradleSuccess opts ->
                      return $ unlines ["Options: " ++ show (componentOptions opts)
                                       ,"ComponentDir: " ++ (componentRoot opts)
                                       ,"Dependencies: " ++ show (componentDependencies opts) ]
                    CradleNone -> return "No flags: this component should not be loaded"
        return (unlines res)

      "help"    -> return usage
      "--help"  -> return usage
      "-h"      -> return usage

      cmd       -> E.throw (NoSuchCommand cmd)
    putStr res
  where
    handlers = [Handler (handleThenExit handler1), Handler (handleThenExit handler2)]
    handleThenExit handler e = handler e >> exitFailure
    handler1 :: ErrorCall -> IO ()
    handler1 = print -- for debug
    handler2 :: HhpcError -> IO ()
    handler2 SafeList = hPutStr stderr usage
    handler2 (TooManyArguments cmd) =
        hPutStrLn stderr $ "\"" ++ cmd ++ "\": Too many arguments"
    handler2 (NotEnoughArguments cmd) = do
        hPutStrLn stderr $ "\"" ++ cmd ++ "\": Not enough arguments"
        hPutStrLn stderr ""
        hPutStr stderr usage
    handler2 (NoSuchCommand cmd) = do
        hPutStrLn stderr $ "\"" ++ cmd ++ "\" not supported"
        hPutStrLn stderr ""
        hPutStr stderr usage
    handler2 (CmdArg errs) =
        mapM_ (hPutStr stderr) errs
    handler2 (FileNotExist file) =
        hPutStrLn stderr $ "\"" ++ file ++ "\" not found"
    xs !. idx
      | length xs <= idx = E.throw SafeList
      | otherwise = xs !! idx
