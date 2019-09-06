module Main (main) where

import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (exitWith)
import System.Process (spawnProcess, waitForProcess)

main = do
  args <- getArgs
  case args of
    "--interactive":_ -> do
      getCurrentDirectory >>= putStrLn
      -- note this probably breaks if paths have spaces in
      putStrLn $ unwords args
    _ -> do
      ph <- spawnProcess "ghc" args
      code <- waitForProcess ph
      exitWith code
