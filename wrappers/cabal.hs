module Main (main) where

import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (exitWith)
import System.Process (spawnProcess, waitForProcess)
import Data.List (nub)

main = do
  args <- getArgs
  case args of
    "--interactive":_ -> do
      getCurrentDirectory >>= putStrLn
      putStrLn $ delimited $ nub args
    _ -> do
      ph <- spawnProcess "ghc" $ nub args
      code <- waitForProcess ph
      exitWith code

delimited :: [String] -> String
delimited = concatMap (++ "\NUL")
