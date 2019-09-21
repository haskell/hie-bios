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
import HIE.Bios.Types
import HIE.Bios.Ghc.Check
import HIE.Bios.Debug
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
        ++ "\t hie-bios debug\n"
        ++ "\t hie-bios root\n"
        ++ "\t hie-bios version\n"

----------------------------------------------------------------

data HhpcError = SafeList
               | TooManyArguments String
               | NoSuchCommand String
               | CmdArg [String]
               | FileNotExist String deriving (Show, Typeable)

instance Exception HhpcError

----------------------------------------------------------------

main :: IO ()
main = flip E.catches handlers $ do
    hSetEncoding stdout utf8
    args <- getArgs
    cradle <- getCurrentDirectory >>= \cwd ->
        -- find cradle does a takeDirectory on the argument, so make it into a file
        findCradle (cwd </> "File.hs") >>= \case
          Just yaml -> loadCradle yaml
          Nothing -> loadImplicitCradle (cwd </> "File.hs")
    let cmdArg0 = args !. 0
        remainingArgs = tail args
        opt = defaultOptions
    res <- case cmdArg0 of
      "check"   -> checkSyntax opt cradle remainingArgs
      "expand"  -> expandTemplate opt cradle remainingArgs
      "debug"   -> debugInfo opt cradle
      "root"    -> rootInfo opt cradle
      "version" -> return progVersion
      "flags"   -> do
        res <- forM remainingArgs $ \fp -> do
                res <- getCompilerOptions fp cradle
                case res of
                    CradleFail (CradleError _ex err) ->
                      return $ "Failed to show flags for \""
                                                ++ fp
                                                ++ "\": " ++ show err
                    CradleSuccess opts -> return $ "CompilerOptions: " ++ show (ghcOptions opts)
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
    handler2 (TooManyArguments cmd) = do
        hPutStrLn stderr $ "\"" ++ cmd ++ "\": Too many arguments"
    handler2 (NoSuchCommand cmd) = do
        hPutStrLn stderr $ "\"" ++ cmd ++ "\" not supported"
        hPutStrLn stderr ""
        hPutStr stderr usage
    handler2 (CmdArg errs) = do
        mapM_ (hPutStr stderr) errs
    handler2 (FileNotExist file) = do
        hPutStrLn stderr $ "\"" ++ file ++ "\" not found"
    xs !. idx
      | length xs <= idx = E.throw SafeList
      | otherwise = xs !! idx
