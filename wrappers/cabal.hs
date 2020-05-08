module Main (main) where

import System.Directory (getCurrentDirectory)
import System.Environment (getArgs, getEnv, lookupEnv)
import System.Exit (exitWith)
import System.Process (spawnProcess, waitForProcess)
import System.IO (openFile, hClose, hPutStrLn, IOMode(..))

main = do
  args <- getArgs
  output_file <- getEnv "HIE_BIOS_OUTPUT"
  case args of
    "--interactive":_ -> do
      h <- openFile output_file AppendMode
      getCurrentDirectory >>= hPutStrLn h
      mapM_ (hPutStrLn h) args
      hClose h
    _ -> do
      ph <- spawnProcess "ghc" (args)
      code <- waitForProcess ph
      exitWith code
