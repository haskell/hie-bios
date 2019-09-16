module HIE.Bios.Debug (debugInfo, rootInfo) where

import Control.Monad.IO.Class (liftIO)

import qualified Data.Char as Char
import Data.Maybe (fromMaybe)

import HIE.Bios.Ghc.Api
import HIE.Bios.Types

----------------------------------------------------------------

-- | Obtaining debug information.
debugInfo :: Options
          -> Cradle
          -> IO String
debugInfo opt cradle = convert opt <$> do
    (_ex, _sterr, gopts) <- getOptions (cradleOptsProg cradle) (cradleRootDir cradle)
    mglibdir <- liftIO getSystemLibDir
    return [
        "Root directory:      " ++ rootDir
      , "GHC options:         " ++ unwords (map quoteIfNeeded gopts)
      , "System libraries:    " ++ fromMaybe "" mglibdir
      ]
  where
    rootDir    = cradleRootDir cradle
    quoteIfNeeded option
      | any Char.isSpace option = "\"" ++ option ++ "\""
      | otherwise = option

----------------------------------------------------------------

-- | Obtaining root information.
rootInfo :: Options
          -> Cradle
          -> IO String
rootInfo opt cradle = return $ convert opt $ cradleRootDir cradle
