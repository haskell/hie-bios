module HIE.Bios.Cradle.Resolved
  ( ResolvedCradles(..)
  , ResolvedCradle(..)
  , ConcreteCradle(..)
  ) where

import HIE.Bios.Cradle.ProgramVersions
import HIE.Bios.Config

-- | The final cradle config that specifies the cradle for
-- each prefix we know how to handle
data ResolvedCradles a = ResolvedCradles
 { cradleRoot :: FilePath
 , resolvedCradles :: [ResolvedCradle a] -- ^ In order of decreasing specificity
 , cradleProgramVersions :: ProgramVersions
 }

-- | 'ConcreteCradle' augmented with information on which file the
-- cradle applies
data ResolvedCradle a = ResolvedCradle
 { prefix :: FilePath -- ^ the prefix to match files
 , cradleDeps :: [FilePath] -- ^ accumulated dependencies
 , concreteCradle :: ConcreteCradle a
 } deriving Show

-- | The actual type of action we will be using to process a file
data ConcreteCradle a
  = ConcreteCabal CabalType
  | ConcreteStack StackType
  | ConcreteBios Callable (Maybe Callable) (Maybe FilePath)
  | ConcreteDirect [String]
  | ConcreteNone
  | ConcreteOther a
  deriving Show

