module HIE.Bios.Cradle.ProjectConfig where

import System.FilePath

-- | Explicit data-type for project configuration location.
-- It is basically a 'Maybe' type, but helps to document the API
-- and helps to avoid incorrect usage.
data CradleProjectConfig
  = NoExplicitConfig
  | ExplicitConfig FilePath
  deriving (Eq, Show)

-- | Create an explicit project configuration. Expects a working directory
-- followed by an optional name of the project configuration.
projectConfigFromMaybe :: FilePath -> Maybe FilePath -> CradleProjectConfig
projectConfigFromMaybe _wdir Nothing = NoExplicitConfig
projectConfigFromMaybe wdir (Just fp) = ExplicitConfig (wdir </> fp)
