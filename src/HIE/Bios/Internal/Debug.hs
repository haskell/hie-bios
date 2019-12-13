module HIE.Bios.Internal.Debug (debugInfo, rootInfo) where

import Control.Monad.IO.Class (liftIO)

import qualified Data.Char as Char
import Data.Maybe (fromMaybe)

import HIE.Bios.Ghc.Api
import HIE.Bios.Types
import HIE.Bios.Flags

----------------------------------------------------------------

-- | Obtain debug information for a 'Cradle'.
--
-- Tries to load the 'Cradle' and dump any information associated with it.
-- If loading succeeds, contains information such as the root directory of
-- the cradle, the compiler options to compile a module in this 'Cradle',
-- the file dependencies and so on.
--
-- Otherwise, shows the error message and exit-code.
debugInfo :: FilePath
          -> Cradle
          -> IO String
debugInfo fp cradle = unlines <$> do
    res <- getCompilerOptions fp cradle
    case res of
      CradleSuccess (ComponentOptions gopts deps) -> do
        mglibdir <- liftIO getSystemLibDir
        return [
            "Root directory:      " ++ rootDir
          , "GHC options:         " ++ unwords (map quoteIfNeeded gopts)
          , "System libraries:    " ++ fromMaybe "" mglibdir
          , "Dependencies:        " ++ unwords deps
          ]
      CradleFail (CradleError ext stderr) ->
        return ["Cradle failed to load"
               , "Exit Code: " ++ show ext
               , "Stderr: " ++ unlines stderr]
      CradleNone ->
        return ["No cradle"]
  where
    rootDir    = cradleRootDir cradle
    quoteIfNeeded option
      | any Char.isSpace option = "\"" ++ option ++ "\""
      | otherwise = option

----------------------------------------------------------------

-- | Get the root directory of the given Cradle.
rootInfo :: Cradle
          -> IO String
rootInfo cradle = return $ cradleRootDir cradle
